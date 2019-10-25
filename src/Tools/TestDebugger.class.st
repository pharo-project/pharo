Class {
	#name : #TestDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'opening api' }
TestDebugger class >> handlesContext: aContext [
	^ true
]

{ #category : #'opening api' }
TestDebugger class >> iAmADebugger [
	<iAmADebugger>
]

{ #category : #'opening api' }
TestDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	Transcript show: 'Opening MyDebugger'
]

{ #category : #'as yet unclassified' }
TestDebugger class >> rank [
	^ 50
]
