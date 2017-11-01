"
I am a critic that provides a selection interval by searching a 'faulty substring' in the target's source code. I also use the 'faulty substring' to enhence the critic's title
"
Class {
	#name : #ReStringBasedCritique,
	#superclass : #ReAbstractCritique,
	#category : 'Renraku-Critiques'
}

{ #category : #'as yet unclassified' }
ReStringBasedCritique class >> for: anEntity by: aRule badString: aString [
	^ self basicNew
		initializeRule: aRule target: anEntity badString: aString;
		yourself 
]

{ #category : #accessing }
ReStringBasedCritique >> change [
	^ self shouldNotImplement
]

{ #category : #initialization }
ReStringBasedCritique >> initializeRule: aRule target: anEntity badString: aString [

	self
		initializeRule: aRule
		sourceAnchor: (
			ReSearchStringSourceAnchor
				entity: anEntity
				string: aString)
]

{ #category : #testing }
ReStringBasedCritique >> providesChange [
	^ false
]

{ #category : #accessing }
ReStringBasedCritique >> title [
	^ '[', sourceAnchor searchString, '] ', super title
]
