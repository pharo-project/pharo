"
I am an abstract class specifying a parameter with nested positional parameters.
"
Class {
	#name : #ClapParameterized,
	#superclass : #ClapParameter,
	#instVars : [
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
	^ aParameter addTo: self
]

{ #category : #adding }
ClapParameterized >> addAll: parameters [
	parameters do: [ :each | each addTo: self ]
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
ClapParameterized >> atName: specName [
	^ self
		positionalNamed: specName
		ifNone: [ nil ]
]

{ #category : #testing }
ClapParameterized >> hasAlias: aString [
	^ aString = self canonicalName or: [ self aliases includes: aString ]
	
]

{ #category : #initialization }
ClapParameterized >> initialize [
	super initialize.
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
ClapParameterized >> positionalNamed: specName ifNone: aBlock [
	^ positionals
		detect: [ :arg | arg canonicalName = specName ]
		ifNone: aBlock
]

{ #category : #accessing }
ClapParameterized >> positionals [
	^ positionals
]
