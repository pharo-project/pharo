"
This class simply implements the Null pattern.
"
Class {
	#name : #GLMNullLogger,
	#superclass : #GLMLogger,
	#category : #'Glamour-Helpers'
}

{ #category : #testing }
GLMNullLogger class >> isDeprecated [
	^ true
]
