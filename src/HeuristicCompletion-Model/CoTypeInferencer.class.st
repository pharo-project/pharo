"
I am a simple type inferencer that is depth-bound.
That is, I infer a method up to a certain level of the callgraph.
"
Class {
	#name : #CoTypeInferencer,
	#superclass : #Object,
	#instVars : [
		'returnType',
		'variables',
		'level',
		'arguments',
		'method',
		'temporaryVariables',
		'receiverClass',
		'argumentVariables'
	],
	#category : #'HeuristicCompletion-Model-InitializeInferenceHeuristic'
}

{ #category : #accessing }
CoTypeInferencer >> arguments: aCollection [
	
	arguments := aCollection
]

{ #category : #inference }
CoTypeInferencer >> cleanupTypes [

	| unknownType |
	unknownType := CoUnknownType new.
	variables valuesDo: [ :e |
		e remove: unknownType ifAbsent: [ "nothing" ] ]
]

{ #category : #visiting }
CoTypeInferencer >> ensureTypeOfArgumentVariable: aVariableName [ 
	
	^ argumentVariables
		at: aVariableName
		ifAbsentPut: [ Set new ]
]

{ #category : #visiting }
CoTypeInferencer >> ensureTypeOfTemporaryVariable: aVariableName [ 
	
	^ temporaryVariables
		at: aVariableName
		ifAbsentPut: [ Set new ]
]

{ #category : #visiting }
CoTypeInferencer >> ensureTypeOfVariable: aVariableName [ 

	^ variables
		at: aVariableName
		ifAbsentPut: [ Set new ]
]

{ #category : #inference }
CoTypeInferencer >> inferFrom: aClass [

	self inferFrom: aClass typeGetters: true
]

{ #category : #inference }
CoTypeInferencer >> inferFrom: aClass typeGetters: aBoolean [

	receiverClass := aClass.
	aClass allInstVarNames do: [ :each |
		self ensureTypeOfVariable: each ].
	self tryInferSelector: #initialize.
	aClass isTestCase ifTrue: [
		self tryInferSelector: #setUp ].
	
	aBoolean ifTrue: [ 
		aClass allInstVarNames do: [ :each |
			self tryInferSelector: each ] ].
	self cleanupTypes
]

{ #category : #visiting }
CoTypeInferencer >> inferMessageReturn: aRBMessageNode receiverType: type argumentTypes: argumentTypes [

	| lookupClass inferer |
	lookupClass := aRBMessageNode receiver isSuper
		ifTrue: [ method methodClass superclass ]
		ifFalse: [ type ].
	method := lookupClass lookupSelector: aRBMessageNode selector.
	method ifNil: [ ^ { CoUnknownType new } ].
	
	"We only explore n levels and we arrived at the last one.
	But we only stop if this is not a conditional.
	"
	(level <= 0 and: [
		(#( ifTrue: ifFalse: ifTrue:ifFalse: ifNil: ifNotNil: ifNil:ifNotNil:) 
			includes: aRBMessageNode selector) not ]) ifTrue: [
				"When we stop, check fast if the looked-up method has no return, and guess self"
				^ method ast body lastIsReturn
					ifTrue: [ { CoUnknownType new } ]
					ifFalse: [ { type } ] ].
	
	inferer := self class new
		level: level;
		arguments: argumentTypes;
		receiverClass: type.
	(receiverClass includesBehavior: type)
		ifTrue: [ inferer variables: variables ].
	^ inferer
		inferMethod: method;
		returnType
]

{ #category : #inference }
CoTypeInferencer >> inferMethod: aMethod [

	method := aMethod.
	aMethod methodClass allInstVarNames do: [ :each |
		self ensureTypeOfVariable: each ].
	method ast acceptVisitor: self
]

{ #category : #initialization }
CoTypeInferencer >> initialize [

	super initialize.
	level := 3.
	arguments := #().
	argumentVariables := Dictionary new.
	temporaryVariables := Dictionary new.
	variables := Dictionary new.
	returnType := OrderedCollection new.
	returnType add: CoUnknownType new
]

{ #category : #accessing }
CoTypeInferencer >> level [
	^ level
]

{ #category : #accessing }
CoTypeInferencer >> level: anObject [
	level := anObject
]

{ #category : #visiting }
CoTypeInferencer >> lookupVariableType: aVariableName [ 
	
	^ variables
		at: aVariableName
		ifAbsent: [ 
			argumentVariables
				at: aVariableName
				ifAbsent: [ self ensureTypeOfTemporaryVariable: aVariableName ] ]
]

{ #category : #accessing }
CoTypeInferencer >> receiverClass: aClass [ 
	receiverClass := aClass
]

{ #category : #accessing }
CoTypeInferencer >> returnType [
	^ returnType
]

{ #category : #visiting }
CoTypeInferencer >> stackMessage: aBlock level: aNewLevel [

	| previousLevel previousMethod previousTemporaries previousArguments |
	previousMethod := method.
	previousLevel := level.
	previousTemporaries := temporaryVariables.
	previousArguments := argumentVariables.
	level := aNewLevel.
	^ aBlock ensure: [ 
		level := previousLevel.
		method := previousMethod.
		temporaryVariables := previousTemporaries.
		argumentVariables := previousArguments ]
]

{ #category : #inference }
CoTypeInferencer >> tryInferSelector: aSelector [

	"Try to infer types from a selector in the current class.
	If the class has no such selector, do nothing."
	
	| foundMethod |
	foundMethod := (receiverClass lookupSelector: #initialize).
	foundMethod ifNil: [ ^ self ].
	self inferMethod: foundMethod
]

{ #category : #accessing }
CoTypeInferencer >> variables [
	^ variables
]

{ #category : #accessing }
CoTypeInferencer >> variables: aCollection [ 
	variables := aCollection
]

{ #category : #visiting }
CoTypeInferencer >> visitArgumentNode: aRBArgumentNode [ 
	
	^ self ensureTypeOfArgumentVariable: aRBArgumentNode name
]

{ #category : #visiting }
CoTypeInferencer >> visitArrayNode: aRBArrayNode [ 
	
	^ { Array }
]

{ #category : #visiting }
CoTypeInferencer >> visitAssignmentNode: aRBAssignmentNode [ 
	
	| types |
	types := self lookupVariableType: aRBAssignmentNode variable name.
	^ types addAll: (aRBAssignmentNode value acceptVisitor: self);
		yourself
]

{ #category : #visiting }
CoTypeInferencer >> visitBlockNode: aRBBlockNode [ 
	
	"Just visit the contents"
	| last |
	last := { CoUnknownType new }.
	aRBBlockNode statements do: [ :each |
		last := each acceptVisitor: self ].
	^ last
]

{ #category : #visiting }
CoTypeInferencer >> visitCascadeNode: aRBCascadeNode [ 
	
	| receiverType last |
	receiverType := aRBCascadeNode receiver acceptVisitor: self.
	aRBCascadeNode messages do: [ :each |
		last := self
			visitMessageNode: each
			receiverType: receiverType ].
	^ last
]

{ #category : #visiting }
CoTypeInferencer >> visitGlobalNode: aRBGlobalNode [ 
	
	| binding |
	binding := aRBGlobalNode binding value.
	^ { binding class }
]

{ #category : #visiting }
CoTypeInferencer >> visitInstanceVariableNode: aRBInstanceVariableNode [ 
	
	^ self ensureTypeOfVariable: aRBInstanceVariableNode name
]

{ #category : #visiting }
CoTypeInferencer >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	
	^ self visitArrayNode: aRBLiteralArrayNode
]

{ #category : #visiting }
CoTypeInferencer >> visitLiteralValueNode: aRBLiteralValueNode [ 
	
	^ { aRBLiteralValueNode value class }
]

{ #category : #visiting }
CoTypeInferencer >> visitMessageNode: aRBMessageNode [ 
	
	| receiverType |
	receiverType := aRBMessageNode receiver acceptVisitor: self.
	^ self
		visitMessageNode: aRBMessageNode
		receiverType: receiverType
]

{ #category : #visiting }
CoTypeInferencer >> visitMessageNode: aRBMessageNode receiverType: receiverType [
	
	| argumentTypes |
	(#(new basicNew new: basicNew:) includes: aRBMessageNode selector)
		ifTrue: [ ^ receiverType collect: [ :each | each instanceSide ] ].
	
	((#( = == > < >= <= and: or:) includes: aRBMessageNode selector)
		or: [ aRBMessageNode selector beginsWith: 'is' ])
			ifTrue: [ ^ { True . False } ].
	
	(#(yourself copy) includes: aRBMessageNode selector)
		ifTrue: [ ^ receiverType ].
	
	(#(+ - / *) includes: aRBMessageNode selector)
		ifTrue: [ ^ { Number } ].
	
	"(aRBMessageNode selector = #asValueHolder)
		ifTrue: [ ^ { NewValueHolder } ]."
	
	aRBMessageNode selector = #class
		ifTrue: [ ^ receiverType collect: [ :each | each class ] ].

	argumentTypes := aRBMessageNode arguments
		collect: [ :each | each acceptVisitor: self ].
		
	^ receiverType gather: [ :type |
		self
			stackMessage: [
				self
					inferMessageReturn: aRBMessageNode
					receiverType: type
					argumentTypes: argumentTypes ]
			level: ((#( initialize setUp ) includes: aRBMessageNode selector)
				ifTrue: [level]
				ifFalse: [level - 1]) ]
]

{ #category : #visiting }
CoTypeInferencer >> visitMethodNode: aRBMethodNode [ 
	
	aRBMethodNode arguments withIndexDo: [ :each :index |
		(self ensureTypeOfArgumentVariable: each name)
			addAll: (arguments at: index)
	].
	aRBMethodNode statements do: [ :each |
		each acceptVisitor: self ].
	aRBMethodNode statements ifNotEmpty: [ :statements |
		statements last isReturn
			ifFalse: [ returnType add: method methodClass ] ]
]

{ #category : #visiting }
CoTypeInferencer >> visitReturnNode: aRBReturnNode [ 
	
	^ returnType 
		addAll: (aRBReturnNode value acceptVisitor: self);
		yourself
]

{ #category : #visiting }
CoTypeInferencer >> visitSelfNode: aRBSelfNode [ 
	
	^ { receiverClass }
]

{ #category : #visiting }
CoTypeInferencer >> visitSuperNode: aRBSuperNode [ 
	
	^ self visitSelfNode: aRBSuperNode
]

{ #category : #visiting }
CoTypeInferencer >> visitTemporaryNode: aRBTemporaryNode [ 
	
	^ self ensureTypeOfTemporaryVariable: aRBTemporaryNode name
]

{ #category : #visiting }
CoTypeInferencer >> visitThisContextNode: aRBThisContextNode [ 
	
	^ { Context }
]
