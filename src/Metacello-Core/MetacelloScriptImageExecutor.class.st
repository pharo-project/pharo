Class {
	#name : 'MetacelloScriptImageExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution callback' }
MetacelloScriptImageExecutor >> projectSpecSelectBlock [
    ^ [ :projectSpec | 
    projectSpec
        ifNil: [ false ]
        ifNotNil: [ 
            MetacelloProjectRegistration
                registrationForProjectSpec: projectSpec
                ifAbsent: [ false ]
                ifPresent: [ :existingRegistration :newRegistration | existingRegistration loadedInImage ] ] ]
]
