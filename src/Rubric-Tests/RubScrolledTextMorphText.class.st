Class {
	#name : #RubScrolledTextMorphText,
	#superclass : #TestCase,
	#category : #'Rubric-Tests'
}

{ #category : #tests }
RubScrolledTextMorphText >> testGhostText [
	| scrolledText | 
	scrolledText := RubScrolledTextMorph new 
				ghostText: 'This is a cool ghost text';
				yourself.
	self assert: scrolledText ghostText equals: 'This is a cool ghost text'
]
