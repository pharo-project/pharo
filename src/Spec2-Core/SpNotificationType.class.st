"
I am a base for notification types (errors, informations)
"
Class {
	#name : #SpNotificationType,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #accessing }
SpNotificationType class >> error [

	^ SpNotificationTypeError uniqueInstance
]

{ #category : #accessing }
SpNotificationType class >> info [

	^ SpNotificationTypeInfo uniqueInstance
]

{ #category : #'instance creation' }
SpNotificationType class >> uniqueInstance [

	self = SpNotificationType ifTrue: [ 
		self error: 'I''m an abstract use one of my children instead' ].
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]

{ #category : #dispatching }
SpNotificationType >> notify: aSpecNotification on: aNotificationAware [
	
	self subclassResponsibility
]
