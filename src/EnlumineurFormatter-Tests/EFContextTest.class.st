Class {
	#name : 'EFContextTest',
	#superclass : 'TestCase',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'tests' }
EFContextTest >> testMultiLineMessagesClassSide [

	| messageList |
	
	messageList := self class compiler evaluate: EFFormatter multiLineMessages.
	self assert: (messageList includes: #ifTrue:).  
	self assert: (messageList includes: #ifFalse:).
]

{ #category : 'tests' }
EFContextTest >> testNumberOfSpaces [

	| context |
	context := EFContext new. 
	self assert: context indentExtraSpaces equals: ''
]

{ #category : 'tests' }
EFContextTest >> testOneLineMessage [

	| context |
	context := EFContext new. 
	self assert: (context oneLineMessages includes: #to:).
	self assert: (context oneLineMessages includes: #to:do:).
]

{ #category : 'tests' }
EFContextTest >> testOneLineMessageClassSide [

	| messageList |
	
	messageList := self class compiler evaluate: EFFormatter oneLineMessages.
	self assert: (messageList includes: #to:).  
	self assert: (messageList includes: #to:do:).

]
