Class {
	#name : #SycConvertTempToinstVarCommandTest,
	#superclass : #TestCase,
	#category : #'SystemCommands-MethodCommands-Tests'
}

{ #category : #tests }
SycConvertTempToinstVarCommandTest >> testCanExecuteOnArgumentNode [
	| argNode sourceCodeContext |
	"we don't need to set the tools"
	argNode := RBArgumentNode new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: argNode.
	self deny: (SycConvertTempToInstVarCommand canBeExecutedInContext: sourceCodeContext).

]

{ #category : #tests }
SycConvertTempToinstVarCommandTest >> testCanExecuteOnTemporaryNode [
	| sourceCodeContext tempNode |
	"we don't need to set the tools"
	tempNode := RBTemporaryNode new.
	sourceCodeContext := ClySourceCodeContext for: nil selectedNode: tempNode.
	self assert: (SycConvertTempToInstVarCommand canBeExecutedInContext: sourceCodeContext).
]
