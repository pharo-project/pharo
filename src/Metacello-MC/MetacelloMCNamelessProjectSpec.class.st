Class {
	#name : #MetacelloMCNamelessProjectSpec,
	#superclass : #MetacelloMCProjectSpec,
	#category : #Metacello-MC-Specs
}

{ #category : #scripting }
MetacelloMCNamelessProjectSpec >> asProjectSpecForVersion: vrsn [
  "I'm a reference spec and my project comes from the project I'm loaded from, not the project I am loading"

  "https://github.com/dalehenrich/metacello-work/issues/331"

  | proj spec |
  proj := vrsn ifNil: [ ^ self ] ifNotNil: [ vrsn versionSpec project ].
  spec := proj configurationOfProjectSpecClass for: proj.
  self copyForScriptingInto: spec.
  spec loader: self loader.
  self assert: (self className beginsWith: 'BaselineOf') not.
  ^ spec
]

{ #category : #mutability }
MetacelloMCNamelessProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [
	self error: 'Should be converting to configuration spec for the registration, so we should not get here'
]
