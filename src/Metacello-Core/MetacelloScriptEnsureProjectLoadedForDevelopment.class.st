Class {
	#name : 'MetacelloScriptEnsureProjectLoadedForDevelopment',
	#superclass : 'MetacelloScriptNotification',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'exception handling' }
MetacelloScriptEnsureProjectLoadedForDevelopment >> defaultAction [
    "Go ahead and download a new copy of configuration because blessing is #development"

    ^ true
]

{ #category : 'handlers' }
MetacelloScriptEnsureProjectLoadedForDevelopment >> handleResolutionFor: aScriptEngine [
    ^ aScriptEngine handleEnsureProjectLoadedForDevelopment: self
]
