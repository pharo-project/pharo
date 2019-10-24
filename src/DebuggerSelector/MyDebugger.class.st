Class {
	#name : #MyDebugger,
	#superclass : #Object,
	#category : #DebuggerSelector
}

{ #category : #'opening api' }
MyDebugger class >> handlesContext: aContext [
	^ true
]

{ #category : #'opening api' }
MyDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	<debugger>
	Transcript show: 'Opening MyDebugger'
]

{ #category : #'as yet unclassified' }
MyDebugger class >> rank [
	^ 50
]
