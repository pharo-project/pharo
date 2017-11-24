"
**MetacelloLoadProjectSpecVersionNotification** is signalled to indicate that the given project spec was loaded into the image.
"
Class {
	#name : #MetacelloProjectSpecLoadedNotification,
	#superclass : #MetacelloScriptProjectSpecNotification,
	#category : #'Metacello-Core-Scripts'
}

{ #category : #'exception description' }
MetacelloProjectSpecLoadedNotification >> defaultAction [
    ^ nil
]

{ #category : #handlers }
MetacelloProjectSpecLoadedNotification >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleProjectSpecLoaded: self
]
