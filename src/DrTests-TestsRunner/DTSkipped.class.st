"
I model the fact that a test was skipped.
"
Class {
	#name : #DTSkipped,
	#superclass : #DTTestResultType,
	#category : #'DrTests-TestsRunner'
}

{ #category : #testing }
DTSkipped >> isSkipped [
	<generated>
	^ true
]

{ #category : #accessing }
DTSkipped >> name [
	^ 'Skipped test'
]
