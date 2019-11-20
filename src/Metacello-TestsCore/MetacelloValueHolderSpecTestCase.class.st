Class {
	#name : #MetacelloValueHolderSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsCore-Specs'
}

{ #category : #tests }
MetacelloValueHolderSpecTestCase >> testValueHolderMergeSpec [
	| valueHolderA valueHolderB valueHolder |
	valueHolderA := self valueHolderSpec
		value: 'an Object';
		yourself.
	valueHolderB := self valueHolderSpec
		value: 1.1;
		yourself.
	valueHolder := valueHolderA mergeSpec: valueHolderB.
	self assert: valueHolder value equals: 1.1
]

{ #category : #tests }
MetacelloValueHolderSpecTestCase >> testValueHolderSpec [
	| valueHolder |
	valueHolder := self valueHolderSpec
		value: 'an Object';
		yourself.
	self assert: valueHolder value equals: 'an Object'
]
