"
Help to debug 
"
Class {
	#name : #InputEventLog,
	#superclass : #InputEventHandler,
	#instVars : [
		'filterBlock'
	],
	#category : #'System-VMEvents-Base'
}

{ #category : #accessing }
InputEventLog >> filter: aBlock [

	filterBlock := aBlock
]

{ #category : #events }
InputEventLog >> handleEvent: anEvent [

	(filterBlock isNil or: [ filterBlock value: anEvent ])
		ifTrue: [ SystemNotification signal: anEvent asString ]
]
