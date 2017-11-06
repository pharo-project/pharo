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

{ #category : #'instance creation' }
ParserNotification class >> name: aString [
	^ (self new setName: aString) signal
]

{ #category : #accessing }
ParserNotification >> defaultAction [
	
	self openMenuIn: 
		[:labels :lines :caption | 
		UIManager default chooseFrom: labels lines: lines title: caption]
]

{ #category : #correcting }
ParserNotification >> openMenuIn: aBlock [
	self subclassResponsibility
]

{ #category : #accessing }
ParserNotification >> setName: aString [
	name := aString
]
