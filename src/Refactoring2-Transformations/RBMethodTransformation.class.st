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
	#category : 'Refactoring2-Transformations-Model',
	#package : 'Refactoring2-Transformations',
	#tag : 'Model'
}

{ #category : 'accessing' }
RBMethodTransformation >> definingMethod [

	^ method ifNil: [
		method := (self definingClass methodFor: selector asSymbol) ast ]
]
