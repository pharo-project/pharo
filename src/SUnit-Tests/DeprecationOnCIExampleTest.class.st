"
I am a test case run by DeprecationOnCITest, which tells me whether I run on the Pharo CI. My test sends a deprecated message whose callers would be rewritten.
"
Class {
	#name : 'DeprecationOnCIExampleTest',
	#superclass : 'TestCase',
	#instVars : [
		'onCI'
	],
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
DeprecationOnCIExampleTest class >> isAbstract [
	"Run only by DeprecationOnCITest, which sets where it runs."

	^ true
]

{ #category : 'helpers' }
DeprecationOnCIExampleTest >> deprecatedMessage [
	"Deprecated long ago, so its callers are rewritten when its default action runs."

	self
		deprecated: 'Part of DeprecationOnCITest'
		in: #Pharo1
		transformWith: '`@receiver deprecatedMessage' -> '`@receiver newMessage'
]

{ #category : 'helpers' }
DeprecationOnCIExampleTest >> newMessage [

	
]

{ #category : 'accessing' }
DeprecationOnCIExampleTest >> onCI: aBoolean [

	onCI := aBoolean
]

{ #category : 'accessing' }
DeprecationOnCIExampleTest >> onPharoCITestingEnvironment [

	^ onCI
]

{ #category : 'tests' }
DeprecationOnCIExampleTest >> testSendsDeprecatedMessage [

	self deprecatedMessage
]
