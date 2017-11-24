"
A utility class containing the help for GT BytecodeDebugger.
"
Class {
	#name : #GTBytecodeDebuggerHelp,
	#superclass : #GTDebuggerHelp,
	#category : #'GT-BytecodeDebugger-Model'
}

{ #category : #accessing }
GTBytecodeDebuggerHelp class >> bookName [ 
	^'Bytecode'
]

{ #category : #accessing }
GTBytecodeDebuggerHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'GT Bytecode is an extension for GT Debugger providing support for stepping through bytecode instructions.'
]

{ #category : #accessing }
GTBytecodeDebuggerHelp class >> pages [
	^#(overview actions)
]
