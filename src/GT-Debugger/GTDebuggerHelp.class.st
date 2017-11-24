"
A utility class containing the help for GT Debugger.

Customs debuggers that want to provide a help topic should create a subclass and override GTMoldableDebugger class>>#helpClass.

I dynamically build and display  the list of actions  available for a debugger.
"
Class {
	#name : #GTDebuggerHelp,
	#superclass : #CustomHelp,
	#category : #'GT-Debugger-Model'
}

{ #category : #documentation }
GTDebuggerHelp class >> actions [
		
	^ HelpTopic 
		title: 'Actions'
		contents: 'TODO'
]

{ #category : #accessing }
GTDebuggerHelp class >> bookName [ 
	^'GT Debugger'
]

{ #category : #accessing }
GTDebuggerHelp class >> key [
	^ self name
]

{ #category : #documentation }
GTDebuggerHelp class >> overview [
	^ HelpTopic 
		title: 'Overview'
		contents: 
'GT Debugger is a moldable debugger that is part of Pharo and was developed as part of the Glamorous Toolkit project (http://gt.moosetechnology.org)'
]

{ #category : #accessing }
GTDebuggerHelp class >> pages [
	^#(overview actions)
]
