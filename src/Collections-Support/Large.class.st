Class {
	#name : #Large,
	#superclass : #Size,
	#category : #'Collections-Support'
}

{ #category : #comparing }
Large >> < other [
	^ false
]

{ #category : #comparing }
Large >> = other [
	^ other class = self class
]

{ #category : #comparing }
Large >> hash [
	^ Size hash bitXor: 99
]
