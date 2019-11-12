"
This class is not actually a debugger, but when asked to open on a debug session, it will look at all the debuggers available in the system and open the best one for the particular DebugSession being opened
"
Class {
	#name : #Debugger,
	#superclass : #Object,
	#category : #'Tools-Debugger'
}

{ #category : #initialization }
Debugger class >> getDebuggerClasses [
	^ (((PragmaCollector filter: [:prg | prg selector = #'iAmADebugger' ]) reset collected) collect: [ :debuggerMethod | debuggerMethod methodClass soleInstance ]) yourself.
]

{ #category : #'opening api' }
Debugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"Get all the debugger classes available in the image and sort them by the rank they give to @aDebugSession (through the #rankDebugSession method). Try to open the first one (highest rank) on @aDebugSession. If an exception is raised doing so, try to open the second one, etc..."
	| listOfDebuggerClassesSortedByRank |
	"Members of listOfDebuggerClassesSortedByRank are of the form [debuggerClass, rank it gave to @aDebugSession]"
	listOfDebuggerClassesSortedByRank := (self getDebuggerClasses collect: [ :aDebuggerClass | { aDebuggerClass. aDebuggerClass rankDebugSession: aDebugSession } ]) sort: [ :val1 :val2 | (val1 at: 2) >= (val2 at: 2) ].
	(listOfDebuggerClassesSortedByRank collect: [ :couple | couple at: 1 ]) do: [ :aDebuggerClass |
		[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. ^ nil] on: Error do: [  ].
		 ]
]
