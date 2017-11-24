Class {
	#name : #MetacelloScriptNotification,
	#superclass : #Notification,
	#category : #'Metacello-Core-Scripts'
}

{ #category : #handlers }
MetacelloScriptNotification >> handleResolutionFor: aScriptEngine [
    self subclassResponsibility
]
