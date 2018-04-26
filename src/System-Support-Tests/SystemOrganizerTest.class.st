"
SUnit tests for SystemOrganizer
"
Class {
	#name : #SystemOrganizerTest,
	#superclass : #TestCase,
	#category : #'System-Support-Tests'
}

{ #category : #tests }
SystemOrganizerTest >> testDefault [
	
	self assert: (SystemOrganizer default == self class environment organization).
]

{ #category : #tests }
SystemOrganizerTest >> testDefaultEnvironment [
	"When no environment is specified, point to the default one"
	
	self assert: (SystemOrganizer new  environment == Smalltalk globals)
]

{ #category : #tests }
SystemOrganizerTest >> testThatExistingPackagenamesDoesNotContainIllegalCharacters [
	| illegalCharacters | 
	
	illegalCharacters := #(  $\ $/ $: $* $? $" $< $> $|  ).
	
	self class environment organization categories do: [ :aPackageName |
		self deny: (aPackageName includesAnyOf: illegalCharacters)
	]
]
