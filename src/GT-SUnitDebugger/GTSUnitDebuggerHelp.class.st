"
A utility class containing the help for GT SUnitDebugger.
"
Class {
	#name : #GTSUnitDebuggerHelp,
	#superclass : #GTDebuggerHelp,
	#category : #'GT-SUnitDebugger'
}

{ #category : #accessing }
GTSUnitDebuggerHelp class >> bookName [ 
	^'SUnit'
]

{ #category : #documentation }
GTSUnitDebuggerHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'GT SUnit is an extension for GT Debugger providing support for debugging tests.'
]

{ #category : #accessing }
GTSUnitDebuggerHelp class >> pages [
	^#(overview actions)
]
