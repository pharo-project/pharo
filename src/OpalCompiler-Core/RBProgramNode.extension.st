Extension { #name : #RBProgramNode }

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asDoit [
	"The VM can only evaluate methods. wrap this ast in a doit MethodNode"
	| methodNode doIt |
	methodNode := RBMethodNode 
		selector: #DoIt
		body: self copy asSequenceNode transformLastToReturn.
	"We need to re-parse so that the AST is in sync with the 
	bytecode to be able to debug the doit. Recompile not needed"
	doIt := RBParser parseMethod: methodNode formattedCode.
	self methodNode ifNotNil: [:myMethod |
		doIt compilationContext: myMethod compilationContext copy ].
		
	^doIt
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asDoitForContext: aContext [
	"The VM can only evaluate methods. wrap this ast in a doitIn MethodNode to evaluate in a context"
	| methodNode doIt |
	methodNode := RBMethodNode 
		selector: #DoItIn:
		arguments: { RBVariableNode named: 'ThisContext' } 
		body: self copy asSequenceNode transformLastToReturn.
	"We need to re-parse so that the AST is in sync with the 
	bytecode to be able to debug the doit. Recompile not needed"
	doIt := RBParser parseMethod: methodNode formattedCode.
	self methodNode ifNotNil: [ :myMethod | 
		doIt compilationContext: myMethod compilationContext copy].
	doIt methodClass: aContext receiver class.
	doIt compilationContext prepareForDoIt: doIt inContext: aContext.
		
	^doIt
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asSequenceNode [
	^RBSequenceNode statements: {self} 
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis.
	
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> doSemanticAnalysisIn: aClass [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysisIn: aClass
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> isClean [
	^ self children allSatisfy: [ :child | child isClean ]
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> owningScope [

	^ parent owningScope 
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> printAsIfCompiledOn: aStream [ 
	aStream nextPutAll: self formattedCode
]

{ #category : #'*opalcompiler-core' }
RBProgramNode >> scope [
	^ self methodOrBlockNode scope
]
