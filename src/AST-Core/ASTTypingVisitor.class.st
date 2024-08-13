"
A very simple and fast type inferer.

I'm a work in progress.

I'm not sound nor complete but my concern is to be usaful and fast when accuracy is not a concern (completion, user feedback, etc.).
I approximate the types to `ASTValueNode` instances, `Variable` instance, and return values of `CompiledMethod`.

On AST nodes, type are stored in the `#type` property. Two instance variable dictionaries are used to store the types of variables and metod returns.

A type is represented with a single class.
`nil` represent the bottom element of the type lattice and `ProtoObject` should be the top element.

I do not have intraprocedural sensitivity (not flow-sensitive, nor path-sensitive).
I have a limited interprocedural capabilities as types of arguments are not propagated to the parameters --- so I'm circumstantialy context-sensitive :P.

A single visitor can be used on multiple methods.
This will help sharing the contextual information on interprocedural analysis (types of ivar for instance).

Instance variables:

* variableTypes <Dictionnary> associate assigned Variable objects with a type
* returnTypes <Dictionnary> associate return of compiled method with a type
* shortcutKernelMessages <True> activate some heuristics based on core seletors introduced in `Kernel` (for `Object`, `Class` or `Collection` for instance).
* dirty <Boolean> used for basic fixed-point analysis when more than on pass is needed
"
Class {
	#name : 'ASTTypingVisitor',
	#superclass : 'RBProgramNodeVisitor',
	#instVars : [
		'variableTypes',
		'shortcutKernelMessages',
		'returnTypes',
		'dirty',
		'cha',
		'unknownMethods'
	],
	#category : 'AST-Core-Type',
	#package : 'AST-Core',
	#tag : 'Type'
}

{ #category : 'benchmarking' }
ASTTypingVisitor class >> bench: trees [

	| report valueNodes milliseconds count visitor steps moreTrees knownMethods |
	report := WriteStream on: ''.
	report
		nextPutAll: 'Number of AST: ';
		print: trees size;
		cr.

	valueNodes := (trees flatCollect: #allChildren) select: #isValue.
	report
		nextPutAll: 'Number of value nodes: ';
		print: valueNodes size;
		cr.

	valueNodes do: [ :node | node removeProperty: #type ifAbsent: [  ] ].
	milliseconds := [ trees do: [ :tree | self new visit: tree ] ] millisecondsToRun.
	report
		nextPutAll: 'Individual typing';
		cr;
		nextPutAll: '  milliseconds ';
		print: milliseconds;
		cr;
		nextPutAll: '  typed nodes: ';
		nextPutAll: (self printTypedNodeCount: valueNodes);
		cr.

	valueNodes do: [ :node | node removeProperty: #type ifAbsent: [  ] ].
	visitor := self new.
	milliseconds := [ trees do: [ :tree | visitor visit: tree ] ] millisecondsToRun.
	count := valueNodes count: [ :x | x hasProperty: #type ].
	report
		nextPutAll: 'Agregated typing';
		cr;
		nextPutAll: '  milliseconds:';
		print: milliseconds;
		cr;
		nextPutAll: '  typed nodes: ';
		nextPutAll: (self printTypedNodeCount: valueNodes);
		cr.

	valueNodes do: [ :node | node removeProperty: #type ifAbsent: [  ] ].
	visitor := self new.
	steps := 0.
	milliseconds := [
	            [
	            steps := steps + 1.
	            visitor dirty: false.
	            trees do: [ :tree | visitor visit: tree ].
	            visitor dirty ] whileTrue ] millisecondsToRun.
	count := valueNodes count: [ :x | x hasProperty: #type ].
	report
		nextPutAll: '+fixed point typing';
		cr;
		nextPutAll: '  duration:';
		print: milliseconds;
		cr;
		nextPutAll: '  steps:';
		print: steps;
		cr;
		nextPutAll: '  typed nodes: ';
		nextPutAll: (self printTypedNodeCount: valueNodes);
		cr.

	valueNodes do: [ :node | node removeProperty: #type ifAbsent: [  ] ].
	visitor := self new.
	visitor cha: true.
	steps := 0.
	milliseconds := [
	            [
	            steps := steps + 1.
	            visitor dirty: false.
	            trees do: [ :tree | visitor visit: tree ].
	            visitor dirty ] whileTrue ] millisecondsToRun.
	count := valueNodes count: [ :x | x hasProperty: #type ].
	report
		nextPutAll: '+CHA';
		cr;
		nextPutAll: '  milliseconds:';
		print: milliseconds;
		cr;
		nextPutAll: '  steps:';
		print: steps;
		cr;
		nextPutAll: '  typed nodes: ';
		nextPutAll: (self printTypedNodeCount: valueNodes);
		cr.

	moreTrees := OrderedCollection newFrom: trees.
	knownMethods := trees collect: [ :tree | tree compiledMethod ] as: Set.
	visitor unknownMethods do: [ :method |
		(knownMethods includes: method) ifFalse: [ 
		moreTrees add: method parseTree ].
		].
	valueNodes do: [ :node | node removeProperty: #type ifAbsent: [  ] ].
	visitor := self new.
	visitor cha: true.
	steps := 0.
	milliseconds := [
	            [
	            steps := steps + 1.
	            visitor dirty: false.
	            moreTrees do: [ :tree | visitor visit: tree ].
	            visitor dirty ] whileTrue ] millisecondsToRun.
	count := valueNodes count: [ :x | x hasProperty: #type ].
	report
		nextPutAll: '+CHA';
		cr;
		nextPutAll: '  milliseconds:';
		print: milliseconds;
		cr;
		nextPutAll: '  steps:';
		print: steps;
		cr;
		nextPutAll: '  typed nodes: ';
		nextPutAll: (self printTypedNodeCount: valueNodes);
		cr.
	valueNodes groupedBy: [ :n | n propertyAt: #type ifAbsent: [ nil ] ].
	^ report contents
]

{ #category : 'printing' }
ASTTypingVisitor class >> printTypedNodeCount: valueNodes [

	| count |
	count := valueNodes count: [ :x | x hasProperty: #type ].
	^ String streamContents: [ :aStream |
		  aStream
			  print: count;
			  nextPutAll: ' (';
			  print: (100.0 * count / valueNodes size) rounded;
			  nextPutAll: '%)' ]
]

{ #category : 'accessing' }
ASTTypingVisitor >> cha [

	^ cha
]

{ #category : 'accessing' }
ASTTypingVisitor >> cha: anObject [

	cha := anObject
]

{ #category : 'accessing' }
ASTTypingVisitor >> dirty [

	^ dirty
]

{ #category : 'accessing' }
ASTTypingVisitor >> dirty: anObject [

	dirty := anObject
]

{ #category : 'analyzing' }
ASTTypingVisitor >> fixedPointAnalysis: aNode [
	"Repeat the analysis until a fixed point is reached.
	Because the analysis is monotonous and the height of the latice is bounded, a fixed point is reachable in finite time."

	[
	dirty := false.
	self visit: aNode.
	dirty ] whileTrue
]

{ #category : 'initialization' }
ASTTypingVisitor >> initialize [

	variableTypes := IdentityDictionary new.
	returnTypes := IdentityDictionary new.
	unknownMethods := Set new.
	shortcutKernelMessages := true.
	cha := false
]

{ #category : 'accessing' }
ASTTypingVisitor >> lookupMethod: aSelector type: recvType [

	| method methods type |
	method := recvType lookupSelector: aSelector.

	"Without CHA (Class Hierarchy Analysis), only look at a sigle method.
	This is not sound (not even monotonous!) because we only consider a sigle static type and a single method,
	at runtime, the concrete receiver type might be numerous and different"
	cha ifFalse: [
		^ returnTypes at: method ifAbsent: [
			  unknownMethods add: method.
			  nil ] ].

	methods := Set new.
	method ifNotNil: [ methods add: method ].

	"Look for potential redefinitions in subclasses (CHA!)"
	recvType subclasses do: [ :c |
		c methodDictionary at: aSelector ifPresent: [ :m | methods add: m ] ].
	methods ifEmpty: [ ^ nil ].

	"Merge all possible return types"
	type := nil.
	methods do: [ :m |
		returnTypes
			at: m
			ifPresent: [ :type2 | type := self merge: type with: type2 ]
			ifAbsent: [ unknownMethods add: m ] ].

	^ type
]

{ #category : 'lattice' }
ASTTypingVisitor >> merge: types [

	^ types reduce: [ :t1 :t2 | self merge: t1 with: t2 ]
]

{ #category : 'lattice' }
ASTTypingVisitor >> merge: type1 with: type2 [
	"Do a join operation on the two type, so return the common super class.
	`nil` behave as a bottom element of a lattice.
	`ProtoObject` should behave as a top element.
	
	`UndefinedObject` is also managed as just above nil but bellow all other types."

	| aSuperclass |
	type1 ifNil: [ ^ type2 ].
	type2 ifNil: [ ^ type1 ].
	type1 == UndefinedObject ifTrue: [ ^ type2 ].
	type2 == UndefinedObject ifTrue: [ ^ type1 ].
	type1 == type2 ifTrue: [ ^ type1 ].

	aSuperclass := type1.
	[ aSuperclass isNotNil ] whileTrue: [
		type2 == aSuperclass ifTrue: [ ^ aSuperclass ].
		(type2 inheritsFrom: aSuperclass) ifTrue: [ ^ aSuperclass ].
		aSuperclass := aSuperclass superclass ].

	self error: 'This should not occurs, unless multiple roots?'
]

{ #category : 'accessing' }
ASTTypingVisitor >> shortcutKernelMessages [

	^ shortcutKernelMessages
]

{ #category : 'accessing' }
ASTTypingVisitor >> shortcutKernelMessages: anObject [

	shortcutKernelMessages := anObject
]

{ #category : 'lattice' }
ASTTypingVisitor >> typeMethod: aCompilerMethod with: aClass [

	| type |
	type := aClass.
	returnTypes at: aCompilerMethod ifPresent: [ :oldType |
		oldType = type ifTrue: [ ^ self ].
		type := self merge: oldType with: type.
		oldType = type ifTrue: [ ^ self ] ].

	dirty := true.
	returnTypes at: aCompilerMethod put: type
]

{ #category : 'lattice' }
ASTTypingVisitor >> typeNode: aNode with: aClass [

	| type |
	type := aClass.
	aNode propertyAt: #type ifPresent: [ :oldType |
		oldType = type ifTrue: [ ^ self ].
		"We expect that the analysis is monotonous, but it might not be always the case.
		Se perform a join to rorce some kind of monotonicity."
		type := self merge: oldType with: type.
		oldType = type ifTrue: [ ^ self ] ].

	dirty := true.
	aNode propertyAt: #type put: type
]

{ #category : 'lattice' }
ASTTypingVisitor >> typeVariable: aVariable with: aClass [

	| type |
	type := aClass.
	variableTypes at: aVariable ifPresent: [ :oldType |
		oldType = type ifTrue: [ ^ self ].
		type := self merge: oldType with: type.
		oldType = type ifTrue: [ ^ self ] ].

	dirty := true.
	variableTypes at: aVariable put: type
]

{ #category : 'accessing' }
ASTTypingVisitor >> unknownMethods [

	^ unknownMethods
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitArrayNode: aLiteralNode [

	super visitArrayNode: aLiteralNode.
	self typeNode: aLiteralNode with: Array
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitAssignmentNode: anAssignmentNode [

	| type variable |
	super visitAssignmentNode: anAssignmentNode.

	type := anAssignmentNode value propertyAt: #type ifAbsent: [ ^ self ].
	self typeNode: anAssignmentNode with: type.

	variable := anAssignmentNode variable variable originalVar.
	self typeVariable: variable with: type
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitBlockNode: aNode [

	super visitBlockNode: aNode.
	self typeNode: aNode with: BlockClosure
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitGlobalNode: aGlobalNode [

	self typeNode: aGlobalNode with: aGlobalNode binding read class
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitLiteralArrayNode: aLiteralNode [

	super visitLiteralArrayNode: aLiteralNode.
	self typeNode: aLiteralNode with: Array
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitLiteralNode: aLiteralNode [

	self typeNode: aLiteralNode with: aLiteralNode value class
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitMessageNode: aNode [

	| recvType type |
	super visitMessageNode: aNode.

	"Fast path for some special Kernel selectors (type of the receiver is not important)"
	shortcutKernelMessages ifTrue: [
		(#( = == ~= ~~ < > <= >= isNil isNotNil isEmpty ) includes:
			 aNode selector) ifTrue: [ ^ self typeNode: aNode with: Boolean ].

		(#( size ) includes: aNode selector) ifTrue: [
			^ self typeNode: aNode with: Integer ] ].

	recvType := aNode receiver propertyAt: #type ifAbsent: [ ^ self ].

	"Fast path for some special Kernel selectors (type of the receiver IS important)"
	shortcutKernelMessages ifTrue: [
		(recvType isMeta and: [
			 #( new new: basicNew basicNew: ) includes: aNode selector ])
			ifTrue: [ ^ self typeNode: aNode with: recvType instanceSide ].

		aNode selector == #yourself ifTrue: [
			^ self typeNode: aNode with: recvType ].
		aNode selector == #class ifTrue: [
			^ self typeNode: aNode with: recvType class ] ].


	type := self lookupMethod: aNode selector type: recvType.
	type ifNotNil: [ self typeNode: aNode with: type ]
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitMethodNode: aNode [

	super visitMethodNode: aNode.
	aNode compiledMethod ifNil: [ ^ self ].

	"Do not polute the return value since the method will fail"
	aNode compiledMethod isSubclassResponsibility ifTrue: [ ^ self ].

	"Fallback at an implicit return self"
	aNode containsReturn ifFalse: [
		self typeMethod: aNode compiledMethod with: aNode methodClass ]
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitReturnNode: aNode [

	| type |
	super visitReturnNode: aNode.

	type := aNode value propertyAt: #type ifAbsent: [ ^ self ].
	aNode methodNode compiledMethod ifNotNil: [ :cm |
		self typeMethod: cm with: type ]
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitSelfNode: aSelfNode [
	self typeNode: aSelfNode with: aSelfNode methodNode methodClass
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitSuperNode: aSuperNode [
	| class |
	class := aSuperNode methodNode methodClass.
	"the type of super of a Trait is not known"
	class isTrait ifTrue: [ ^self ].
	self typeNode: aSuperNode with: class superclass
]

{ #category : 'visiting' }
ASTTypingVisitor >> visitVariableNode: aNode [

	| type variable |
	aNode isError ifTrue: [ ^ self ].
	variable := aNode variable originalVar.
	type := variableTypes at: variable ifAbsent: [ ^ self ].
	self typeNode: aNode with: type.
]
