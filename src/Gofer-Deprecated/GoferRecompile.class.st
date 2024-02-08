"
Recompile
"
Class {
	#name : 'GoferRecompile',
	#superclass : 'GoferWorking',
	#category : 'Gofer-Deprecated-Operations',
	#package : 'Gofer-Deprecated',
	#tag : 'Operations'
}

{ #category : 'running' }
GoferRecompile >> execute [

	self workingCopies
		do: [ :each | self execute: each ]
]

{ #category : 'running' }
GoferRecompile >> execute: aWorkingCopy [

	aWorkingCopy systemPackage ifNotNil: [ :package | package methods do: [ :each | each methodClass recompile: each selector ] ]
]
