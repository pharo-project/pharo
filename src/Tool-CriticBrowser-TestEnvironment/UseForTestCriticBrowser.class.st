Class {
	#name : #UseForTestCriticBrowser,
	#superclass : #Object,
	#instVars : [
		'B',
		'A'
	],
	#classInstVars : [
		'Test'
	],
	#category : #'Tool-CriticBrowser-TestEnvironment'
}

{ #category : #accessing }
UseForTestCriticBrowser class >> test [
	^ Test
]

{ #category : #accessing }
UseForTestCriticBrowser >> a [
	^ A
]

{ #category : #accessing }
UseForTestCriticBrowser >> a: anObject [
	A := anObject
]

{ #category : #accessing }
UseForTestCriticBrowser >> b [
	^ B
]

{ #category : #accessing }
UseForTestCriticBrowser >> b: anObject [
	B := anObject
]

{ #category : #example }
UseForTestCriticBrowser >> stringUseInsteadOfStream [
	^ 'abc' , 'def' , 'ghi'
]
