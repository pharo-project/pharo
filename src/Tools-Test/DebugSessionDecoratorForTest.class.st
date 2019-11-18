Class {
	#name : #DebugSessionDecoratorForTest,
	#superclass : #Object,
	#instVars : [
		'nbOfTimesTerminateWasCalled',
		'decoratedDebugSession'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #'instance creation' }
DebugSessionDecoratorForTest class >> newOn: aDebugSession [
	| newInstance |
	newInstance := self new.
	newInstance decoratedDebugSession: aDebugSession.
	^ newInstance
]

{ #category : #accessing }
DebugSessionDecoratorForTest >> decoratedDebugSession: aDebugSession [
	decoratedDebugSession := aDebugSession
]

{ #category : #initialization }
DebugSessionDecoratorForTest >> doesNotUnderstand: aMessage [
	aMessage lookupClass: decoratedDebugSession class.
	^ aMessage sentTo: decoratedDebugSession 
]

{ #category : #initialization }
DebugSessionDecoratorForTest >> initialize [
	super initialize.
	nbOfTimesTerminateWasCalled := 0
]

{ #category : #initialization }
DebugSessionDecoratorForTest >> nbOfTimesTerminateWasCalled [
	^ nbOfTimesTerminateWasCalled
]

{ #category : #initialization }
DebugSessionDecoratorForTest >> terminate [
	nbOfTimesTerminateWasCalled := nbOfTimesTerminateWasCalled + 1.
	^ decoratedDebugSession terminate
]
