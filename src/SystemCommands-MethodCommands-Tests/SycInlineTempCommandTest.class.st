"
Tests that SycInlineTempCommand can run when a temporary is selected in a method editor, but not an argument.
"
Class {
	#name : 'SycInlineTempCommandTest',
	#superclass : 'TestCase',
	#category : 'SystemCommands-MethodCommands-Tests-Tests',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SycInlineTempCommandTest >> testCanExecuteOnArgumentNode [

	| argNode sourceCodeContext |
	"we don't need to set the tools"
	argNode := OCVariableNode new variable: ArgumentVariable new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: argNode.
	self deny: (SycInlineTempCommand new
			 context: sourceCodeContext;
			 yourself) canBeExecuted
]

{ #category : 'tests' }
SycInlineTempCommandTest >> testCanExecuteOnTemporaryNode [

	| sourceCodeContext tempNode |
	"we don't need to set the tools"
	tempNode := OCVariableNode new variable: TemporaryVariable new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: tempNode.
	self assert: (SycInlineTempCommand new
			 context: sourceCodeContext;
			 yourself) canBeExecuted
]
