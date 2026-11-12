"
Tests that SycRenameArgOrTempCommand can run when a temporary or an argument is selected in a method editor.
"
Class {
	#name : 'SycRenameArgOrTempCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycRenameArgOrTempCommandTest >> testCanExecuteOnArgumentNode [

	| argNode sourceCodeContext |
	"we don't need to set the tools"
	argNode := OCVariableNode new variable: ArgumentVariable new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: argNode.
	self assert: (SycRenameArgOrTempCommand new
			 context: sourceCodeContext;
			 yourself) canBeExecuted
]

{ #category : 'tests' }
SycRenameArgOrTempCommandTest >> testCanExecuteOnTemporaryNode [

	| sourceCodeContext tempNode |
	"we don't need to set the tools"
	tempNode := OCVariableNode new variable: TemporaryVariable new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: tempNode.
	self assert: (SycRenameArgOrTempCommand new
			 context: sourceCodeContext;
			 yourself) canBeExecuted
]
