Extension { #name : #ASTBlockNode }

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> bcToASTCache [
	^ bcToASTCache ifNil: [ bcToASTCache := OCBytecodeToASTCache generateForNode: self ]
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> ir [
	"if the ir is not yet set, generate the IR for the whole method, it fills the property here"
	self propertyAt: #ir ifAbsent: [ self methodNode generateIR  ].
	^self propertyAt: #ir
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode.
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> irInstruction [
	^ self parent methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> isClean [
	"a block is clean if it has no escaping vars, has no non local return and its children are clean"
	self isInlined ifTrue: [ ^ false ].
	self scope hasEscapingVars ifTrue: [ ^ false ].
	self hasBlockReturn ifTrue: [ ^ false ].
	^ super isClean
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> isInlined [
	parent isMessage ifFalse: [ ^ false ].
	parent isInlineAndOr ifTrue: [^ true].
	parent isInlineIf ifTrue: [^ true].
	parent isInlineIfNil ifTrue: [^ true].
	^ self isInlinedLoop
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> isInlinedLoop [

	parent isMessage ifFalse: [ ^ false ].
	parent isInlineToDo ifTrue: [^ true].
	parent isInlineWhile ifTrue: [^ true].
	parent isInlineTimesRepeat ifTrue: [^ true].
	parent isInlineRepeat ifTrue: [ ^true ].
	(parent parent isDynamicArray and: [parent parent parent isMessage and: [parent parent parent isInlineCase]]) ifTrue: [^ true].
	parent isInlineCase ifTrue: [^ true].  "otherwise branch"
	^ false
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> owningScope [

	^ self scope ifNil: ["inlined" ^ parent owningScope]
]

{ #category : #'*OpalCompiler-Core' }
ASTBlockNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]
