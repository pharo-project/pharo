Extension { #name : #RBProgramNode }

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asDoItForContext: aContext [
	"The VM can only evaluate methods. wrap this ast in a doitIn MethodNode to evaluate in a context"

	^ self asDoit semanticScope:
		  (OCContextualDoItSemanticScope targetingContext: aContext)
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asDoit [
	"The VM can only evaluate methods. wrap this ast in a doit MethodNode"
	| methodNode |

	methodNode := RBMethodNode
		selector: #DoIt
		body: self asSequenceNode transformLastToReturn.
	methodNode source: methodNode formattedCode.
	^methodNode
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> asSequenceNode [
	^RBSequenceNode statements: {self}
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> doSemanticAnalysisIn: aClass [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysisIn: aClass
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> isAccessingSelf [
	"return true if accessing an ivar, self or super"
	^ self children anySatisfy: [ :child | child isAccessingSelf ]
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> owningScope [

	^ parent owningScope
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> scope [
	^ self methodOrBlockNode scope
]
