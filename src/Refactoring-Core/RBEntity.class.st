"
I'm the root of the model classes.
"
Class {
	#name : 'RBEntity',
	#superclass : 'Object',
	#instVars : [
		'changeFactory'
	],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'initialization' }
RBEntity >> initialize [
	super initialize.
	changeFactory := RBRefactoryChangeManager changeFactory
]

{ #category : 'initialization' }
RBEntity >> parserClass [

	^ OCParser
]

{ #category : 'factory access' }
RBEntity >> rbClassClass [
	^ RBClass
]

{ #category : 'factory access' }
RBEntity >> rbMetaclassClass [
	^ RBMetaclass
]

{ #category : 'factory access' }
RBEntity >> rbMethodClass [
	^ RBMethod
]

{ #category : 'factory access' }
RBEntity >> rbPackageClass [

	^ RBPackage
]

{ #category : 'factory access' }
RBEntity >> rbTraitClass [
	^ RBTrait
]

{ #category : 'factory access' }
RBEntity >> rbTraitedMetaclassClass [
	^ RBTraitedMetaclass 
]
