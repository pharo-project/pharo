"
I'm useful when there is no debugger loaded (i.e. no debugger registered in tools: Smalltalk tools debugger).
I log errors to the Transcript.
"
Class {
	#name : #NoDebugger,
	#superclass : #Object,
	#category : #'Debugging-Core'
}

{ #category : #'class initialization' }
NoDebugger class >> initialize [
	self register
]

{ #category : #logging }
NoDebugger class >> logStringFor: debugSession withNotification: notificationString [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: debugSession name;
			cr;
			nextPutAll: 'debugging process: ';
			nextPutAll: debugSession interruptedProcess name;
			cr;
			nextPutAll: 'context:';
			cr;
			print: debugSession interruptedContext.
		notificationString ifNotNil: [ stream nextPutAll: notificationString ] ].
]

{ #category : #'opening api' }
NoDebugger class >> openOn: debugSession withFullView: bool andNotification: notificationString [
	"No debugger to open, just display the notification on Transcript"
	Transcript show:
		(self logStringFor: debugSession withNotification: notificationString)
]

{ #category : #'tools registry' }
NoDebugger class >> register [
	Smalltalk tools register: self as: #debugger
]
