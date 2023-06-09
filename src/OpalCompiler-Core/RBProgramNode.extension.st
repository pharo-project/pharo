Extension { #name : #RBProgramNode }

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
RBProgramNode >> isClean [
	^ self children allSatisfy: [ :child | child isClean ]
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> owningScope [

	^ parent owningScope
]

{ #category : #'*OpalCompiler-Core' }
RBProgramNode >> scope [
	^ self methodOrBlockNode scope
]
