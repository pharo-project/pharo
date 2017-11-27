Class {
	#name : #Small,
	#superclass : #Size,
	#category : #'Collections-Support'
}

{ #category : #comparing }
Small >> < other [
	^ other class ~= self class
]

{ #category : #comparing }
Small >> = other [
	^ other class = self class
]

{ #category : #comparing }
Small >> hash [
	^ Size hash bitXor: 10
]
