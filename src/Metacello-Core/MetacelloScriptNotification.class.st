Class {
	#name : 'MetacelloScriptNotification',
	#superclass : 'Notification',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'handlers' }
MetacelloScriptNotification >> handleResolutionFor: aScriptEngine [
    self subclassResponsibility
]
