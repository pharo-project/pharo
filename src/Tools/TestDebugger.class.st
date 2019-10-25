Class {
	#name : #TestDebugger,
	#superclass : #AbstractDebugger,
	#category : #'Tools-Debugger'
}

{ #category : #'opening api' }
TestDebugger class >> handlesContext: aContext [
	^ true
]

{ #category : #'debugger declaration' }
TestDebugger class >> iAmADebugger [
	<iAmADebugger>
]

{ #category : #'opening api' }
TestDebugger class >> openOn: aDebugSession withFullView: aBool andNotification: aString [
	Transcript show: 'Opening MyDebugger'
]

{ #category : #'debugger declaration' }
TestDebugger class >> rank [
	^ 50
]
