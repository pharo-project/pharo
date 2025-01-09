Extension { #name : 'OCProgramNode' }

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> doSemanticAnalysis [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> doSemanticAnalysisIn: aClass [
	self methodNode ifNil: [ ^self ].
	^ self methodNode doSemanticAnalysisIn: aClass
]

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> irInstruction [
	^ self methodOrBlockNode ir firstInstructionMatching: [:instr | instr sourceNode == self ]
]

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> isAccessingSelf [
	"return true if accessing an ivar, self or super"
	^ self children anySatisfy: [ :child | child isAccessingSelf ]
]

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> owningScope [

	^ parent owningScope
]

{ #category : '*OpalCompiler-Core' }
OCProgramNode >> scope [
	^ self methodOrBlockNode scope
]
