"
I am a debugger that will do nothing in case I am called.
"
Class {
	#name : 'NoDebugger',
	#superclass : 'AbstractDebugger',
	#category : 'Tools-Debugger',
	#package : 'Tools',
	#tag : 'Debugger'
}

{ #category : 'opening api' }
NoDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	"Do nothing"
]
