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

	^ MetacelloProjectRegistry registrationNamed: projectSpec ifPresent: [ :existingRegistration | existingRegistration loadedInImage ] ifAbsent: [ false ]
]
