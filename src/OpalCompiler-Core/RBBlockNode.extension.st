Extension { #name : 'RBBlockNode' }

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> bcToASTCache [
	^ bcToASTCache ifNil: [ bcToASTCache := OCBytecodeToASTCache generateForNode: self ]
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> firstPcForNode: aNode [

	^ self bcToASTCache firstBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> ir [
	"if the ir is not yet set, generate the IR for the whole method, it fills the property here"
	self propertyAt: #ir ifAbsent: [ self methodNode generateIR  ].
	^self propertyAt: #ir
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> irInstruction [
	^ self parent methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> isClean [
	"a block is clean if..."
	"it or any embedded blocks do not need self"
	self isAccessingSelf ifTrue: [ ^ false ].
	"it or any embedded blocks do not need the outer context to return"
	self hasNonLocalReturn ifTrue: [ ^ false ].
	"there are no escaping vars accessed from the outer"
	self scope hasEscapingVars ifTrue: [ ^ false ].
	^true

]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> isInlined [
	parent isMessage ifFalse: [ ^ false ].
	parent methodCompilationContextOrNil ifNotNil:[:c | c optionInlineNone ifTrue: [ ^false ]].
	parent isInlineAndOr ifTrue: [^ true].
	parent isInlineIf ifTrue: [^ true].
	parent isInlineIfNil ifTrue: [^ true].
	^ self isInlinedLoop
]

{ #category : '*OpalCompiler-Core' }
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

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> lastPcForNode: aNode [

	^ self bcToASTCache lastBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> owningScope [

	^ self scope ifNil: ["inlined" ^ parent owningScope]
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> pcsForNode: aNode [

	^ self bcToASTCache pcsForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> resetBcToASTCache [

	bcToASTCache := nil
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]

{ #category : '*OpalCompiler-Core' }
RBBlockNode >> sourceNodeForPC: anInteger usingBcToASTCache: aBcToASTCache [

	^ aBcToASTCache nodeForPC: anInteger
]
