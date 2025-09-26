Class {
	#name : 'MetacelloNamelessProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'scripting' }
MetacelloNamelessProjectSpec >> asProjectSpecForVersion: vrsn [
  "I'm a reference spec and my project comes from the project I'm loaded from, not the project I am loading"

  "https://github.com/dalehenrich/metacello-work/issues/331"

  | proj spec |
  proj := vrsn ifNil: [ ^ self ] ifNotNil: [ vrsn versionSpec project ].
  spec := MetacelloConfigurationOfProjectSpec for: proj.
  self copyForScriptingInto: spec.
  self assert: (self className beginsWith: 'BaselineOf') not.
  ^ spec
]

{ #category : 'mutability' }
MetacelloNamelessProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
	self error: 'Should be converting to configuration spec for the registration, so we should not get here'
]
