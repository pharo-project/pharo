Class {
	#name : #RBEntity,
	#superclass : #Object,
	#instVars : [
		'modelFactory'
	],
	#category : #'Refactoring-Core-Model'
}

{ #category : #initialization }
RBEntity >> initialize [ 
	super initialize.
	modelFactory := RBClassModelFactory new. 
]
