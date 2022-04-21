Extension { #name : #ASTProgramNode }

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> asDoit [
	"The VM can only evaluate methods. wrap this ast in a doit MethodNode"
	| methodNode |
	
	methodNode := ASTMethodNode 
		selector: #DoIt
		body: self asSequenceNode transformLastToReturn.
	methodNode source: methodNode formattedCode.
	^methodNode
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> asDoitForContext: aContext [
	"The VM can only evaluate methods. wrap this ast in a doitIn MethodNode to evaluate in a context"
	| methodNode |
	
	methodNode := ASTMethodNode 
		selector: #DoItIn:
		arguments: { ASTVariableNode named: 'ThisContext' } 
		body: self asSequenceNode transformLastToReturn.
	
	methodNode methodClass: aContext receiver class.
	methodNode rewriteTempsForContext: aContext.
	methodNode source: methodNode formattedCode.
	^methodNode
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> asSequenceNode [
	^ASTSequenceNode statements: {self}
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis.
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> doSemanticAnalysisIn: aClass [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysisIn: aClass
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> isClean [
	^ self children allSatisfy: [ :child | child isClean ]
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> owningScope [

	^ parent owningScope
]

{ #category : #'*OpalCompiler-Core' }
ASTProgramNode >> scope [
	^ self methodOrBlockNode scope
]
