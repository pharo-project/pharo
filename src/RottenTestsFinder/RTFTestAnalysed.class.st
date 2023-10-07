"
I am the notification signaled once a test has been checked by the RottenTestsFinder.
"
Class {
	#name : 'RTFTestAnalysed',
	#superclass : 'Notification',
	#instVars : [
		'compiledMethod'
	],
	#category : 'RottenTestsFinder-Notifications',
	#package : 'RottenTestsFinder',
	#tag : 'Notifications'
}

{ #category : 'instance creation' }
RTFTestAnalysed class >> compiledMethod: aCompiledMethod [
	^ self new
		compiledMethod: aCompiledMethod;
		yourself
]

{ #category : 'signalling' }
RTFTestAnalysed class >> signalTest: aCompiledMethod [
	(self compiledMethod: aCompiledMethod) signal
]

{ #category : 'accessing' }
RTFTestAnalysed >> compiledMethod [
	^ compiledMethod
]

{ #category : 'accessing' }
RTFTestAnalysed >> compiledMethod: anObject [
	compiledMethod := anObject
]
