Class {
	#name : 'MetacelloScriptImageExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution callback' }
MetacelloScriptImageExecutor >> checkCanRunOn: aProjectSpec [
	"A script of Metacello image runs only on projects loaded in the image"

	(MetacelloProjectRegistry registrationNamed: aProjectSpec className ifPresent: [ :each | each loadedInImage ] ifAbsent: [ false ])
		ifFalse: [ NotFound signal: 'Project ' , aProjectSpec name , ' is not loaded in the image' ]
]
