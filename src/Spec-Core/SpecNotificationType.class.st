"
I am a base for notification types (errors, informations)
"
Class {
	#name : #SpecNotificationType,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #accessing }
SpecNotificationType class >> error [

	^ SpecNotificationTypeError uniqueInstance
]

{ #category : #accessing }
SpecNotificationType class >> info [

	^ SpecNotificationTypeInfo uniqueInstance
]

{ #category : #'instance creation' }
SpecNotificationType class >> uniqueInstance [

	self = SpecNotificationType ifTrue: [ 
		self error: 'I''m an abstract use one of my children instead' ].
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]

{ #category : #dispatching }
SpecNotificationType >> notify: aSpecNotification on: aNotificationAware [
	
	self subclassResponsibility
]
