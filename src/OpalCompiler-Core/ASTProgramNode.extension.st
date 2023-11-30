Extension { #name : 'ASTProgramNode' }

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> doSemanticAnalysisIn: aClass [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysisIn: aClass
]

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> isAccessingSelf [
	"return true if accessing an ivar, self or super"
	^ self children anySatisfy: [ :child | child isAccessingSelf ]
]

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> owningScope [

	^ parent owningScope
]

{ #category : '*OpalCompiler-Core' }
ASTProgramNode >> scope [
	^ self methodOrBlockNode scope
]
