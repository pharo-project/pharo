Class {
	#name : #EFContextTest,
	#superclass : #TestCase,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #tests }
EFContextTest >> testNumberOfSpaces [

	| context |
	context := EFContext new. 
	self assert: context indentExtraSpaces equals: ''
]
