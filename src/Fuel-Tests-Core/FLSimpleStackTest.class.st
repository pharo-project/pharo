Class {
	#name : #FLSimpleStackTest,
	#superclass : #TestCase,
	#category : #'Fuel-Tests-Core-Collections'
}

{ #category : #testing }
FLSimpleStackTest >> testCapacity [
	| stack |
	stack := FLSimpleStack new: 3.
	self assert: 3 equals: stack capacity.

	stack := FLSimpleStack new: 5.
	self assert: 5 equals: stack capacity
]

{ #category : #'tests-adding' }
FLSimpleStackTest >> testGrow [
	| stack |
	stack := FLSimpleStack new: 1.
	self assert: 1 equals: stack capacity.
	stack push: #a.
	self assert: 1 < stack capacity
]

{ #category : #testing }
FLSimpleStackTest >> testIsEmpty [
	self assert: FLSimpleStack new isEmpty
]

{ #category : #testing }
FLSimpleStackTest >> testPop [
	| stack |
	stack := FLSimpleStack new.
	stack push: 1.
	stack pop.
	self assert: stack isEmpty
]

{ #category : #testing }
FLSimpleStackTest >> testPush [

	| stack |
	stack := FLSimpleStack new.
	stack push: 1.
	self deny: stack isEmpty.
]
