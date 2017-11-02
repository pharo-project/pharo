"
Used for compiling and decompiling brace constructs.

These now compile into either a fast short form for 4 elements or less:
	Array braceWith: a with: b ... 
or a long form of indefinfite length:
	(Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray.

The erstwhile brace assignment form is no longer supported.
"
Class {
	#name : #BraceNode,
	#superclass : #ParseNode,
	#instVars : [
		'elements',
		'sourceLocations',
		'emitNode'
	],
	#category : 'Compiler-ParseNodes'
}

{ #category : #examples }
BraceNode class >> example [
	"Test the {a. b. c} syntax."

	| x |
	x := {1. {2. 3}. 4}.
	^ {x first. x second first. x second last. x last. 5} as: Set

"BraceNode example Set (0 1 2 3 4 5 )"

]

{ #category : #visiting }
BraceNode >> accept: aVisitor [
	^aVisitor visitBraceNode: self
]

{ #category : #'code generation (closures)' }
BraceNode >> analyseTempsWithin: scopeBlock "<BlockNode>" rootNode: rootNode "<MethodNode>" assignmentPools: assignmentPools [ "<Dictionary>"
	elements do:
		[:node|
		node analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools]
]

{ #category : #testing }
BraceNode >> blockAssociationCheck: encoder [
	"If all elements are MessageNodes of the form [block]->[block], and there is at
	 least one element, answer true.
	 Otherwise, notify encoder of an error."

	elements size = 0
		ifTrue: [^encoder notify: 'At least one case required'].
	elements with: sourceLocations do:
			[:x :loc |
			(x 	isMessage: #->
				receiver:
					[:rcvr |
					rcvr isBlockNode and: [rcvr numberOfArguments = 0]]
				arguments:
					[:arg |
					arg isBlockNode and: [arg numberOfArguments = 0]])
			  ifFalse:
				[^encoder notify: 'Association between 0-argument blocks required' at: loc]].
	^true
]

{ #category : #enumerating }
BraceNode >> casesForwardDo: aBlock [
	"For each case in forward order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	1 to: (numCases := elements size) do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]
]

{ #category : #enumerating }
BraceNode >> casesReverseDo: aBlock [
	"For each case in reverse order, evaluate aBlock with three arguments:
	 the key block, the value block, and whether it is the last case."

	| numCases case |
	(numCases := elements size) to: 1 by: -1 do:
		[:i |
		case := elements at: i.
		aBlock value: case receiver value: case arguments first value: i=numCases]
]

{ #category : #'code generation (closures)' }
BraceNode >> elements [
	^elements
]

{ #category : #initialize-release }
BraceNode >> elements: collection [
	"Decompile."

	elements := collection
]

{ #category : #initialize-release }
BraceNode >> elements: collection sourceLocations: locations [
	"Compile."

	elements := collection.
	sourceLocations := locations
]

{ #category : #'code generation' }
BraceNode >> emitCodeForValue: stack encoder: encoder [

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[elements do: [:node| node emitCodeForValue: stack encoder: encoder].
		 encoder genPushConsArray: elements size.
		 stack
			pop: elements size;
			push: 1.
		 ^self].
	^emitNode emitCodeForValue: stack encoder: encoder
]

{ #category : #initialize-release }
BraceNode >> matchBraceStreamReceiver: receiver messages: messages [

	((receiver isMessage: #braceStream: receiver: nil arguments: [:arg | arg isConstantNumber])
		and: [messages last isMessage: #braceArray receiver: nil arguments: nil])
		ifFalse: [^ nil "no match"].

	"Appears to be a long form brace construct"
	self elements: (messages allButLast collect:
		[:msg | (msg isMessage: #nextPut: receiver: nil arguments: nil)
					ifFalse: [^ nil "not a brace element"].
		msg arguments first])
]

{ #category : #initialize-release }
BraceNode >> matchBraceWithReceiver: receiver selector: selector arguments: arguments [

	selector = (self selectorForShortForm: arguments size)
		ifFalse: [^ nil "no match"].

	"Appears to be a short form brace construct"
	self elements: arguments
]

{ #category : #'code generation (closures)' }
BraceNode >> maxElementsForConsArray [
	"Hack; we have no way of knowing how much stack space is available during sizing"
	^8
]

{ #category : #testing }
BraceNode >> numElements [

	^ elements size
]

{ #category : #printing }
BraceNode >> printOn: aStream indent: level [

	aStream nextPut: ${.
	1 to: elements size do: 
		[:i | (elements at: i) printOn: aStream indent: level.
		i < elements size ifTrue: [aStream nextPutAll: '. ']].
	aStream nextPut: $}
]

{ #category : #'code generation' }
BraceNode >> selectorForShortForm: nElements [

	nElements > 4 ifTrue: [^ nil].
	^ #(braceWithNone braceWith: braceWith:with:
			braceWith:with:with: braceWith:with:with:with:) at: nElements + 1
]

{ #category : #'code generation' }
BraceNode >> sizeCodeForValue: encoder [

	(encoder supportsClosureOpcodes
		"Hack; we have no way of knowing how much stack space is available"
	 and: [elements size <= self maxElementsForConsArray]) ifTrue:
		[^(elements inject: 0 into: [:sum :node| sum + (node sizeCodeForValue: encoder)])
		  + (encoder sizePushConsArray: elements size)].
	emitNode := elements size <= 4
		ifTrue: ["Short form: Array braceWith: a with: b ... "
				MessageNode new
					receiver: (encoder encodeVariable: #Array)
					selector: (self selectorForShortForm: elements size)
					arguments: elements precedence: 3 from: encoder]
		ifFalse: ["Long form: (Array braceStream: N) nextPut: a; nextPut: b; ...; braceArray"
				CascadeNode new
					receiver: (MessageNode new
								receiver: (encoder encodeVariable: #Array)
								selector: #braceStream:
								arguments: (Array with: (encoder encodeLiteral: elements size))
								precedence: 3 from: encoder)
					messages: ((elements collect: [:elt | MessageNode new receiver: nil
														selector: #nextPut:
														arguments: (Array with: elt)
														precedence: 3 from: encoder])
								copyWith: (MessageNode new receiver: nil
														selector: #braceArray
														arguments: (Array new)
														precedence: 1 from: encoder))].
	^emitNode sizeCodeForValue: encoder
]
