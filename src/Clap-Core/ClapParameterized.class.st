"
I am an abstract class specifying a parameter with nested positional parameters.
"
Class {
	#name : #ClapParameterized,
	#superclass : #ClapParameter,
	#instVars : [
		'parameters',
		'positionals',
		'aliases'
	],
	#category : #'Clap-Core-Specification'
}

{ #category : #testing }
ClapParameterized class >> isAbstract [
	^ self == ClapParameterized
]

{ #category : #adding }
ClapParameterized >> add: aParameter [
	parameters
		at: aParameter identifier
		ifPresent: [ :param | self signalIdentifierConflictOf: aParameter with: param ]
		ifAbsentPut: aParameter.
	^ aParameter addTo: self
]

{ #category : #adding }
ClapParameterized >> addAll: parametersToAdd [
	parametersToAdd do: [ :each | each addTo: self ]
]

{ #category : #adding }
ClapParameterized >> addPositional: aPositional [
	positionals add: aPositional
]

{ #category : #accessing }
ClapParameterized >> aliases [
	^ aliases
]

{ #category : #initialization }
ClapParameterized >> aliases: aCollection [
	aliases := aCollection
]

{ #category : #accessing }
ClapParameterized >> at: identifierOrSpec [
	^ parameters at: identifierOrSpec asClapIdentifier
]

{ #category : #accessing }
ClapParameterized >> at: identifierOrSpec ifAbsent: absentBlock [
	^ parameters at: identifierOrSpec asClapIdentifier
		ifAbsent: absentBlock
]

{ #category : #accessing }
ClapParameterized >> at: identifierOrSpec ifPresent: presentBlock [
	^ parameters
		at: identifierOrSpec asClapIdentifier
		ifPresent: presentBlock
]

{ #category : #accessing }
ClapParameterized >> at: identifierOrSpec ifPresent: presentBlock ifAbsent: absentBlock [
	^ parameters
		at: identifierOrSpec asClapIdentifier
		ifPresent: presentBlock
		ifAbsent: absentBlock
]

{ #category : #accessing }
ClapParameterized >> atName: aName [
	self
		deprecated: 'Querying by name or specification has been unified.'
		transformWith: '`@receiver atName: `@name'
			-> '`@receiver at: `@name'.
	^ self at: aName
]

{ #category : #testing }
ClapParameterized >> hasAlias: aString [
	^ aString = self canonicalName or: [ self aliases includes: aString ]
	
]

{ #category : #initialization }
ClapParameterized >> initialize [
	super initialize.
	parameters := OrderedDictionary new.
	positionals := OrderedCollection new.
	aliases := #()
]

{ #category : #accessing }
ClapParameterized >> matchClass [
	^ ClapNamedMatch
]

{ #category : #enumerating }
ClapParameterized >> parametersDo: aBlock [
	positionals do: aBlock
]

{ #category : #accessing }
ClapParameterized >> positionals [
	^ positionals
]

{ #category : #private }
ClapParameterized >> signalIdentifierConflictOf: newParameter with: currentParameter [
	Error signal: 'Identifier conflict (' , newParameter identifier , ')'
]
