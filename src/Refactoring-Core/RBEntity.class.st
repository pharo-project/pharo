"
I'm the root of the model classes.
"
Class {
	#name : 'RBEntity',
	#superclass : 'Object',
	#instVars : [
		'modelFactory',
		'changeFactory'
	],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'initialization' }
RBEntity >> initialize [
	super initialize.
	modelFactory := RBClassModelFactory new.
	changeFactory := RBRefactoryChangeManager changeFactory
]

{ #category : 'initialization' }
RBEntity >> parserClass [

	^ RBParser
]
