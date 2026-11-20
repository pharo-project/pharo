"
I am a test case run by DeprecationAsErrorTest. My test sends a deprecated message whose callers would be rewritten.
"
Class {
	#name : 'DeprecationAsErrorExampleTest',
	#superclass : 'TestCase',
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
DeprecationAsErrorExampleTest class >> isAbstract [
	"Run only by DeprecationAsErrorTest, which sets how deprecations react."

	^ true
]

{ #category : 'helpers' }
DeprecationAsErrorExampleTest >> deprecatedMessage [
	"Deprecated long ago, so its callers are rewritten when its default action runs."

	self
		deprecated: 'Part of DeprecationOnCITest'
		in: #Pharo1
		transformWith: '`@receiver deprecatedMessage' -> '`@receiver newMessage'
]

{ #category : 'helpers' }
DeprecationAsErrorExampleTest >> deprecatedMessageInItsPeriod [
	"Deprecated in the current version: still in its compatibility period."

	self
		deprecated: 'Part of DeprecationAsErrorTest'
		in: 'Pharo' , SystemVersion current major printString
		transformWith: '`@receiver deprecatedMessageInItsPeriod' -> '`@receiver newMessage'
]

{ #category : 'helpers' }
DeprecationAsErrorExampleTest >> newMessage [

	
]

{ #category : 'tests' }
DeprecationAsErrorExampleTest >> testSendsDeprecatedMessage [

	self deprecatedMessage
]

{ #category : 'tests' }
DeprecationAsErrorExampleTest >> testSendsDeprecatedMessageInItsPeriod [

	self deprecatedMessageInItsPeriod
]
