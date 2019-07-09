"
self new open
"
Class {
	#name : #GLMSimpleRubricExample,
	#superclass : #GLMRubricExample,
	#category : #'Glamour-Rubric-Presentations-Examples'
}

{ #category : #accessing }
GLMSimpleRubricExample >> presentation [
	^ GLMCompositePresentation new
		with: [ :a | 
			| t |
			t := a text.
			t withLineNumbers: true.
			t withAnnotation: true.
			t wrapped: true.
			t tabWidth: 40.
			t withColumns: true]
]
