Class {
	#name : 'MetacelloProjectSpecForLoad',
	#superclass : 'Object',
	#instVars : [
		'projectSpec',
		'overrideProjectSpec'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'testing' }
MetacelloProjectSpecForLoad >> hasOverride [

	^ self overrideProjectSpec isNotNil
]

{ #category : 'accessing' }
MetacelloProjectSpecForLoad >> overrideProjectSpec [
	^ overrideProjectSpec
]

{ #category : 'accessing' }
MetacelloProjectSpecForLoad >> overrideProjectSpec: anObject [
	overrideProjectSpec := anObject
]

{ #category : 'accessing' }
MetacelloProjectSpecForLoad >> projectSpec [
	^ projectSpec
]

{ #category : 'accessing' }
MetacelloProjectSpecForLoad >> projectSpec: anObject [
	projectSpec := anObject
]
