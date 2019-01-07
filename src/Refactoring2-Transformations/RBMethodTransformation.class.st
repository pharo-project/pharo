"
I'm a transformation whose focus is a method. 
"
Class {
	#name : #RBMethodTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'method',
		'selector'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #accessing }
RBMethodTransformation >> definingMethod [

	^ method ifNil: [ 
		method := (self definingClass methodFor: selector asSymbol) ast ]
]
