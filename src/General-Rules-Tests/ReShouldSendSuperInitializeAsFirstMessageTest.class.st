"
A ReDoNotSendSuperInitializeInClassSideRuleTest is a test class for testing the behavior of ReDoNotSendSuperInitializeInClassSideRule
"
Class {
	#name : 'ReShouldSendSuperInitializeAsFirstMessageTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Clean Code',
	#package : 'General-Rules-Tests',
	#tag : 'Clean Code'
}

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleNotViolated [

	self generateMethod: 'initialize super initialize. 3+4' classified: 'initialization'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #initialize)
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleNotViolatedWithTemps [

	self generateMethod: 'initialize |a| super initialize. a := 3+4' classified: 'initialization'.
	self assertEmpty: (self myCritiquesOnMethod: self class >> #initialize)
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleViolated [

	self generateMethod: 'initialize 3+4' classified: 'initialization'.
	self assert: (self myCritiquesOnMethod: self class >> #initialize) first rule class equals: ReShouldSendSuperInitializeAsFirstMessage
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleViolatedWithTemps [

	self generateMethod: 'initialize |a| a := 3+4' classified: 'initialization'.
	self assert: (self myCritiquesOnMethod: self class >> #initialize) first rule class equals: ReShouldSendSuperInitializeAsFirstMessage
]
