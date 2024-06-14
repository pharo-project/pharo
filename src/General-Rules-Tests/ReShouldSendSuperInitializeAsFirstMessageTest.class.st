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

	| critiques |
	self class
		compile: 'initialize super initialize. 3+4'
		classified: 'initialization'.
	[
	critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assertEmpty: critiques ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleNotViolatedWithTemps [

	| critiques |
	self class
		compile: 'initialize |a| super initialize. a := 3+4'
		classified: 'initialization'.
	[
	critiques := self myCritiquesOnMethod: self class >> #initialize.
	self assertEmpty: critiques ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleViolated [

	| critiques |
	self class
		compile: 'initialize 3+4'
		classified: 'initialization'.
	[
	critiques := self myCritiquesOnMethod: self class >> #initialize.
		self assert: critiques first rule class equals: ReShouldSendSuperInitializeAsFirstMessage  ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]

{ #category : 'tests' }
ReShouldSendSuperInitializeAsFirstMessageTest >> testRuleViolatedWithTemps [

	| critiques |
	self class
		compile: 'initialize |a| a := 3+4'
		classified: 'initialization'.
	[
	critiques := self myCritiquesOnMethod: self class >> #initialize.
		self assert: critiques first rule class equals: ReShouldSendSuperInitializeAsFirstMessage  ] ensure: [
		(self class >> #initialize) removeFromSystem ]
]
