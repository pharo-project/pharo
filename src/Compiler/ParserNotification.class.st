"
Common superclass for parser notifications
"
Class {
	#name : #ParserNotification,
	#superclass : #Notification,
	#instVars : [
		'name'
	],
	#category : #Compiler-Exceptions
}

{ #category : #'as yet unclassified' }
ParserNotification class >> name: aString [
	^ (self new setName: aString) signal
]

{ #category : #'as yet unclassified' }
ParserNotification >> defaultAction [
	
	self openMenuIn: 
		[:labels :lines :caption | 
		UIManager default chooseFrom: labels lines: lines title: caption]
]

{ #category : #'as yet unclassified' }
ParserNotification >> openMenuIn: aBlock [
	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ParserNotification >> setName: aString [
	name := aString
]
