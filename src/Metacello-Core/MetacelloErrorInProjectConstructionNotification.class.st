Class {
	#name : #MetacelloErrorInProjectConstructionNotification,
	#superclass : #Notification,
	#instVars : [
		'versionString',
		'exception'
	],
	#category : #Metacello-Core-Exceptions-Notifications
}

{ #category : #'instance creation' }
MetacelloErrorInProjectConstructionNotification class >> versionString: aString exception: anException [
    ^ self new
        versionString: aString;
        exception: anException;
        signal
]

{ #category : #'exception description' }
MetacelloErrorInProjectConstructionNotification >> defaultAction [
    "Answer false if you want the version recorded in the errorMap.
	Answer true if you want to go ahead and throw the error"

    ^ false
]

{ #category : #accessing }
MetacelloErrorInProjectConstructionNotification >> exception [
	^ exception
]

{ #category : #accessing }
MetacelloErrorInProjectConstructionNotification >> exception: anObject [
	exception := anObject
]

{ #category : #accessing }
MetacelloErrorInProjectConstructionNotification >> versionString [
	^ versionString
]

{ #category : #accessing }
MetacelloErrorInProjectConstructionNotification >> versionString: anObject [
	versionString := anObject
]
