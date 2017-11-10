Extension { #name : #RBBlockNode }

{ #category : #'*opalcompiler-core' }
RBBlockNode >> enclosingMethodOrBlockNode [
	^ parent ifNotNil: [ parent methodOrBlockNode ]
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> hasBlockReturn [
	^ self body lastIsReturn
]

{ #category : #'*OpalCompiler-Core' }
RBBlockNode >> ir [

	^ self propertyAt: #ir ifAbsent: [ self parent methodOrBlockNode ir ]
]

{ #category : #'*OpalCompiler-Core' }
RBBlockNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode.
]

{ #category : #'*OpalCompiler-Core' }
RBBlockNode >> irInstruction [
	^ self parent methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> isClean [
	"a block is clean if it has no escaping vars, has no non local return and its children are clean"
	self isInlined ifTrue: [ ^ false ].
	self scope hasEscapingVars ifTrue: [ ^ false ].
	self hasBlockReturn ifTrue: [ ^ false ].
	^ super isClean
	
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> isInlined [
	parent isMessage ifFalse: [ ^ false ].
	parent isInlineAndOr ifTrue: [^ true].
	parent isInlineIf ifTrue: [^ true].
	parent isInlineIfNil ifTrue: [^ true].
	^ self isInlinedLoop
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> isInlinedLoop [
	parent isMessage ifFalse: [ ^ false ].
	parent isInlineToDo ifTrue: [^ true].
	parent isInlineWhile ifTrue: [^ true].
	parent isInlineTimesRepeat ifTrue: [^ true].
	parent isInlineRepeat ifTrue: [ ^true ].
	(parent parent isDynamicArray and: [parent parent parent isMessage and: [parent parent parent isInlineCase]]) ifTrue: [^ true].
	parent isInlineCase ifTrue: [^ true].  "otherwise branch"
	^ false
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> lastIsReturn [
	^ false
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> methodOrBlockNode [
	"^ self"
]

{ #category : #'*opalcompiler-core' }
RBBlockNode >> owningScope [

	^ self scope ifNil: ["inlined" ^ parent owningScope]
]

{ #category : #'*OpalCompiler-Core' }
RBBlockNode >> sourceNodeExecutedForPC: anInteger [
	self methodNode ir.
	(self hasProperty: #ir) ifTrue: [ "FullBlockClosure"
		^(self ir fullBlockInstructionForPC: anInteger) sourceNodeExecuted ].
	^ self methodNode sourceNodeExecutedForPC: anInteger
]

{ #category : #'*OpalCompiler-Core' }
RBBlockNode >> sourceNodeForPC: anInteger [ 
	self methodNode ir.
	(self hasProperty: #ir) ifTrue: [ "FullBlockClosure"
		^(self ir fullBlockInstructionForPC: anInteger) sourceNode ].
	^ self methodNode sourceNodeForPC: anInteger
]
