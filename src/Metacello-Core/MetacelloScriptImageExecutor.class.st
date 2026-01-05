Class {
	#name : 'MetacelloScriptImageExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution callback' }
MetacelloScriptImageExecutor >> shouldBeExecutedFor: projectSpec [

	projectSpec ifNil: [ ^ false ].

	^ MetacelloProjectRegistration
		  registrationForProjectSpec: projectSpec
		  ifPresent: [ :existingRegistration | existingRegistration loadedInImage ]
		  ifAbsent: [ false ]
]
