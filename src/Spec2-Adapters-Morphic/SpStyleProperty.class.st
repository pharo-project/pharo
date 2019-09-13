Class {
	#name : #SpStyleProperty,
	#superclass : #SpStyle,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #operations }
SpStyleProperty >> applyTo: aMorph [
	
]

{ #category : #'instance creation' }
SpStyleProperty >> fromSton: stonReader [

	"I will use accessors because I want to be able to add convenience methods"
	stonReader parseMapDo: [ :message :value |
		self 
			perform: message asMutator 
			with: value ]
]

{ #category : #testing }
SpStyleProperty >> isStyleProperty [

	^ true
]
