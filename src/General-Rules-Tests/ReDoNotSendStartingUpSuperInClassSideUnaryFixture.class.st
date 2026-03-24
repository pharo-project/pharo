Class {
	#name : 'ReDoNotSendStartingUpSuperInClassSideUnaryFixture',
	#superclass : 'Object',
	#instVars : [
		'selector'
	],
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'instance creation' }
ReDoNotSendStartingUpSuperInClassSideUnaryFixture class >> onSelector: aSelector [

    ^ self new
          selector: aSelector;
          yourself
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideUnaryFixture >> methodWithSuper [

    ^ selector , ' super ' , selector , '. 2 + 2'
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideUnaryFixture >> methodWithoutSuper [

    ^ selector , ' 2 + 2'
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideUnaryFixture >> methodWithoutSuperWrongSelector [

    ^ 'hola' , ' super ' , selector , '. 2 + 2'
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideUnaryFixture >> selector: aSelector [

    selector := aSelector
]
