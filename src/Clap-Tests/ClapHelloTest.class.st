Class {
	#name : #ClapHelloTest,
	#superclass : #ClapPharoCommandsTest,
	#category : #'Clap-Tests-Commands'
}

{ #category : #tests }
ClapHelloTest >> testHello [
	context := ClapCommandLineExamples hello activateWith: #('hello').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'hello, world.' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloBothShoutingAndWhispering [
	context := ClapCommandLineExamples hello activateWith: #('hello' '--shout' '--whisper').
	
	self skip: 'should probably fail or obey the latest flag'
]

{ #category : #tests }
ClapHelloTest >> testHelloFrench [
	context := ClapCommandLineExamples hello activateWith: #('hello' '--language' 'fr').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'bonjour, tout le monde.' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloShoutingLong [
	context := ClapCommandLineExamples hello activateWith: #('hello' '--shout').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'HELLO, WORLD!' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloShoutingShort [
	context := ClapCommandLineExamples hello activateWith: #('hello' '-s').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'HELLO, WORLD!' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloSomeone [
	context := ClapCommandLineExamples hello activateWith: #('hello' 'someone').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: 'hello, someone.' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloWhisperingLong [
	context := ClapCommandLineExamples hello activateWith: #('hello' '--whisper').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '(hello, world)' , self lineEnding
]

{ #category : #tests }
ClapHelloTest >> testHelloWhisperingShort [
	context := ClapCommandLineExamples hello activateWith: #('hello' '-w').
	
	self assertSuccess.
	self
		assert: self outputString
		equals: '(hello, world)' , self lineEnding
]
