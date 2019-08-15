Class {
	#name : #RubScrolledTextMorphTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests'
}

{ #category : #tests }
RubScrolledTextMorphTest >> testGhostText [
	| scrolledText | 
	scrolledText := RubScrolledTextMorph new 
				ghostText: 'This is a cool ghost text';
				yourself.
	self assert: scrolledText ghostText string equals: 'This is a cool ghost text'
]
