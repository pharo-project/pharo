"
GLMFormatedPresentation is an abstract presentation that offers a format block to be used for formatting the rendering of the presentation. The renderer will use the formatDisplayValueOf: method.
"
Class {
	#name : #GLMFormatedPresentation,
	#superclass : #GLMPresentation,
	#instVars : [
		'format',
		'icon'
	],
	#category : 'Glamour-Core'
}

{ #category : #accessing }
GLMFormatedPresentation >> format [
	^ format ifNil: [ format := #asString ]
]

{ #category : #accessing }
GLMFormatedPresentation >> format: anObject [ 
	 
	format := anObject
]

{ #category : #accessing }
GLMFormatedPresentation >> formatedDisplayValue [
	^ self formatedDisplayValueOf: self displayValue
]

{ #category : #accessing }
GLMFormatedPresentation >> formatedDisplayValueOf: anObject [
	^ self format glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)
]

{ #category : #accessing }
GLMFormatedPresentation >> icon [
	^ icon ifNil: [ icon := [nil] ]
]

{ #category : #accessing }
GLMFormatedPresentation >> icon: anObject [
	icon := anObject
]

{ #category : #accessing }
GLMFormatedPresentation >> iconFor: anObject [
	^ self icon glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)
]
