Class {
	#name : #FluidClassBuilderAbstractTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'FluidClassBuilder-Tests-Base'
}

{ #category : #accessing }
FluidClassBuilderAbstractTest >> builder [

	^ builder
]

{ #category : #accessing }
FluidClassBuilderAbstractTest >> packageNameForTest [

	^ #FakedCore
]

{ #category : #running }
FluidClassBuilderAbstractTest >> tearDown [

	(Smalltalk organization packageNamed: self packageNameForTest ifAbsent: [ nil ]) ifNotNil: [ :x | x removeFromSystem ].

	super tearDown
]
