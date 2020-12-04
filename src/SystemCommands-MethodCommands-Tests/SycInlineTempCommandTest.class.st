Class {
	#name : #SycInlineTempCommandTest,
	#superclass : #TestCase,
	#category : #'SystemCommands-MethodCommands-Tests'
}

{ #category : #tests }
SycInlineTempCommandTest >> testCanExecuteOnArgumentNode [
	| argNode sourceCodeContext |
	"we don't need to set the tools"
	argNode := RBArgumentNode new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: argNode.
	self deny: (SycInlineTempCommand canBeExecutedInContext: sourceCodeContext).

]

{ #category : #tests }
SycInlineTempCommandTest >> testCanExecuteOnTemporaryNode [
	| sourceCodeContext tempNode |
	"we don't need to set the tools"
	tempNode := RBTemporaryNode new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: tempNode.
	self assert: (SycInlineTempCommand canBeExecutedInContext: sourceCodeContext).
]
