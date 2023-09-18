Class {
	#name : #TextMorphTest,
	#superclass : #ClassTestCase,
	#category : #'Morphic-Tests-Basic'
}

{ #category : #coverage }
TextMorphTest >> classToBeTested [

	^ TextMorph
]

{ #category : #testing }
TextMorphTest >> testInitialize [
	"For now, just make sure initialization doesn't throw exception"

	TextMorph initialize
]
