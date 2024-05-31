"
I'm a transformation whose focus is a method. 
"
Class {
	#name : 'RBMethodTransformation',
	#superclass : 'RBClassTransformation',
	#instVars : [
		'method',
		'selector'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'accessing' }
RBMethodTransformation >> definingMethod [

	^ method ifNil: [
		method := (self definingClass methodFor: selector asSymbol) ast ]
]
