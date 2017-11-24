"
Used to signal progress without requiring a specific receiver to notify. Caller/callee convention could be to simply count the number of signals caught or to pass more substantive information with #signal:.
"
Class {
	#name : #ProgressNotification,
	#superclass : #Notification,
	#instVars : [
		'amount',
		'done',
		'extra'
	],
	#category : #'UIManager-Support'
}

{ #category : #exceptioninstantiator }
ProgressNotification class >> signal: signalerText extra: extraParam [
	"TFEI - Signal the occurrence of an exceptional condition with a specified textual description."

	| ex |
	ex := self new.
	ex extraParam: extraParam.
	^ex signal: signalerText
]

{ #category : #accessing }
ProgressNotification >> amount [
	^amount
]

{ #category : #accessing }
ProgressNotification >> amount: aNumber [
	amount := aNumber
]

{ #category : #accessing }
ProgressNotification >> done [
	^done
]

{ #category : #accessing }
ProgressNotification >> done: aNumber [
	done := aNumber
]

{ #category : #accessing }
ProgressNotification >> extraParam [
	^extra
]

{ #category : #accessing }
ProgressNotification >> extraParam: anObject [
	extra := anObject
]
