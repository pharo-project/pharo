"
I represent the non-occurrence of a parameter in the argument sequence.

My instances are only created on demand in response to a query for a parameter that was not recognized in the argument sequence, i.e. it was omitted.
When evaluated with #value or #value:, my meaning is given by the implicit meaning block of my specification.
"
Class {
	#name : #ClapImplicit,
	#superclass : #ClapSubExpression,
	#category : #'Clap-Core-Activation'
}

{ #category : #'instance creation' }
ClapImplicit class >> of: aParameter in: parentMatch [
	^ (self specification: aParameter)
		parent: parentMatch;
		yourself
]

{ #category : #enumerating }
ClapImplicit >> allOccurrences [
	^ Array with: self
]

{ #category : #enumerating }
ClapImplicit >> allOccurrencesCollect: aBlock [
	^ Array with: (aBlock value: self)
]

{ #category : #enumerating }
ClapImplicit >> allOccurrencesDo: aBlock [
	aBlock value: self
]

{ #category : #testing }
ClapImplicit >> isExplicit [
	^ false
]

{ #category : #testing }
ClapImplicit >> isValid [
	self flag: 'could be false if resulted from wrong access...'.
	^ true
]

{ #category : #evaluating }
ClapImplicit >> value: arg [
	^ specification valueForImplicit: self with: arg
]
