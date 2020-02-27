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

{ #category : #tests }
EFContextTest >> testOneLineMessage [

	| context |
	context := EFContext new. 
	self assert: (context oneLineMessages includes: #to:).
	self assert: (context oneLineMessages includes: #to:do:).
	
]

{ #category : #tests }
EFContextTest >> testOneLineMessageClassSide [

	self assert: (EFFormatter oneLineMessages includes: #to:).  
	self assert: (EFFormatter oneLineMessages includes: #to:do:).
	
]
