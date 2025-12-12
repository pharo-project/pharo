Class {
	#name : 'MetacelloNamelessProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloNamelessProjectSpec class >> initialize [

	"Deprecation added during the development of Pharo 14."
	self deprecatedAliases: #( #MetacelloMCNamelessProjectSpec )
]

{ #category : 'mutability' }
MetacelloNamelessProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
	self error: 'Should be converting to configuration spec for the registration, so we should not get here'
]
