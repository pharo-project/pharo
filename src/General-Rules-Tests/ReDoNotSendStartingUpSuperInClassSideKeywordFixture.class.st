Class {
	#name : 'ReDoNotSendStartingUpSuperInClassSideKeywordFixture',
	#superclass : 'Object',
	#instVars : [
		'selector'
	],
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'instance creation' }
ReDoNotSendStartingUpSuperInClassSideKeywordFixture class >> onSelector: aSelector [

    ^ self new
          selector: aSelector;
          yourself
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideKeywordFixture >> methodWithSuper [

    ^ selector , ' aBoolean super ' , selector , ' aBoolean. 2 + 2'
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideKeywordFixture >> methodWithoutSuper [

    ^ selector , ' aBoolean ' , ' 2 + 2'
]

{ #category : 'tests' }
ReDoNotSendStartingUpSuperInClassSideKeywordFixture >> methodWithoutSuperWrongSelector [

    ^ 'hola: aBoolean' , ' super ' , selector , ' aBoolean.' , ' 2 + 2'
]

{ #category : 'accessing' }
ReDoNotSendStartingUpSuperInClassSideKeywordFixture >> selector: aSelector [

    selector := aSelector
]
