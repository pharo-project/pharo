Class {
	#name : #MyDebugger,
	#superclass : #Object,
	#category : #'Tools-Debugger'
}

{ #category : #'opening api' }
MyDebugger class >> handlesContext: aContext [
	^ true
]

{ #category : #'opening api' }
MyDebugger class >> iAmADebugger [
	<iAmADebugger>
]

{ #category : #'opening api' }
MyDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	Transcript show: 'Opening MyDebugger'
]

{ #category : #'as yet unclassified' }
MyDebugger class >> rank [
	^ 50
]
