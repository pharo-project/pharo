"
I'm an external property of some entity. I have a source anchor to it…
"
Class {
	#name : #ReProperty,
	#superclass : #Model,
	#instVars : [
		'sourceAnchor'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #actions }
ReProperty >> actions [ 

	^ #()
]

{ #category : #accessing }
ReProperty >> color [

	^ Color black alpha: 0.1
]

{ #category : #accessing }
ReProperty >> entity [
	^ self sourceAnchor entity
]

{ #category : #accessing }
ReProperty >> icon [

	^ Form extent: 16@16 depth: 32
]

{ #category : #initialization }
ReProperty >> initializeSourceAnchor: anAnchor [

	self initialize.
	sourceAnchor := anAnchor
]

{ #category : #deprecated }
ReProperty >> isCritic [

	self
		deprecated: 'Please use #isCritique instead'
		transformWith: '`@receiver isCritic' -> '`@receiver isCritique'.
	^ self isCritique
]

{ #category : #testing }
ReProperty >> isCritique [
	^ false
]

{ #category : #accessing }
ReProperty >> sourceAnchor [
	^ sourceAnchor
]

{ #category : #accessing }
ReProperty >> title [

	^ self subclassResponsibility 
]
