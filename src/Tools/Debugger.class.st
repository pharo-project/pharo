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
	"Open @aDebugSession with the most suitable debugger class in the system. Look at the comments below for the detailed algorithm"
	| debuggerClasses |
	debuggerClasses := self getDebuggerClasses. "Get all debugger classes in the system"
	debuggerClasses := debuggerClasses select: [:aDebuggerClass | [aDebuggerClass rankDebugSession: aDebugSession. true] on: DebugSessionCannotBeOpenedByThisDebugger do: [false]]. "Exclude all debugger classes that signal the DebugSessionCannotBeOpenedByThisDebugger exception when asked to rank @aDebugSession, as these cannot open @aDebugSession"
	debuggerClasses := ((debuggerClasses collect: [ :aDebuggerClass | { aDebuggerClass. aDebuggerClass rankDebugSession: aDebugSession } ]) sort: [ :val1 :val2 | (val1 at: 2) >= (val2 at: 2) ]) collect: [:couple | couple at: 1]. "Ask each debugger class to rank @aDebugSession, and sort them from highest rank to lowest."
	debuggerClasses do: [ :aDebuggerClass |
		[aDebuggerClass openOn: aDebugSession withFullView: aBool andNotification: aString. ^ nil] on: Error do: [ "Trying to open this debugger raised an error, so we stay in the loop and try the next highest-ranking debugger" ]
	]. "Try to open @aDebugSession with the highest ranking debugger. If this raises an Error, try with the second highest ranking debugger instead. If this raises an Error, try with the third highest..."
	self inform: 'No debugger could open this debug session'. 
]
