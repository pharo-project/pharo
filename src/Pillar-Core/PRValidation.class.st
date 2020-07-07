"
During construction of the parameters, just after parsing 
there is an obscure validation phase. 
And I'm the class holding a strategy of our this validation is handled.
"
Class {
	#name : #PRValidation,
	#superclass : #DynamicVariable,
	#category : #'Pillar-Core-Validations'
}

{ #category : #protocol }
PRValidation class >> beSilentDuring: aBlock [
	^ self 
			value: PRValidationStrategy silent
			during: aBlock
]

{ #category : #accessing }
PRValidation class >> defaultStrategy [
	^ PRValidationStrategy shouting
]

{ #category : #accessing }
PRValidation class >> strategy [
	^ PRValidation value ifNil: [ self defaultStrategy ]
]
