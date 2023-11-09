"
I am a scope of projects which shows only objects which projects extend.
I see all extended classes and extension methods
"
Class {
	#name : 'ClyProjectExtensionScope',
	#superclass : 'ClyProjectScope',
	#category : 'Calypso-SystemQueries-Scopes',
	#package : 'Calypso-SystemQueries',
	#tag : 'Scopes'
}

{ #category : 'queries' }
ClyProjectExtensionScope >> classesDo: aBlock [

	self packagesDo: [ :package |
		package extendedClasses
			collect: [ :each | each instanceSide ]
			thenDo: aBlock ]
]

{ #category : 'queries' }
ClyProjectExtensionScope >> methodsDo: aBlock [

	self packagesDo: [ :package | package extensionMethods do: aBlock ]
]
