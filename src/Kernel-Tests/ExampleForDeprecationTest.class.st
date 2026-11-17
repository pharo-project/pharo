"
I am a class used by DeprecationTest
"
Class {
	#name : 'ExampleForDeprecationTest',
	#superclass : 'Object',
	#category : 'Kernel-Tests-Exceptions',
	#package : 'Kernel-Tests',
	#tag : 'Exceptions'
}

{ #category : 'helpers' }
ExampleForDeprecationTest >> deprecatedMessageWithTransform [
	"Deprecated in the previous Pharo version: this class is not kernel, so its compatibility period ends now."

	self
		deprecated: 'This method is part of the DeprecationTest.'
		on: '1/1/2026'
		in: 'Pharo' , (SystemVersion current major - 1) printString
		transformWith: '`@receiver deprecatedMessageWithTransform' -> '`@receiver newMessage'
]

{ #category : 'helpers' }
ExampleForDeprecationTest >> deprecatedMessageWithTransformAndNoVersion [

	self
		deprecated: 'This method is part of the DeprecationTest.'
		transformWith: '`@receiver deprecatedMessageWithTransformAndNoVersion' -> '`@receiver newMessage'
]

{ #category : 'helpers' }
ExampleForDeprecationTest >> deprecatedMessageWithTransformInCompatibilityPeriod [
	"Deprecated in the current Pharo version: its compatibility period lasts one more version."

	self
		deprecated: 'This method is part of the DeprecationTest.'
		on: '1/1/2026'
		in: 'Pharo' , SystemVersion current major printString
		transformWith: '`@receiver deprecatedMessageWithTransformInCompatibilityPeriod' -> '`@receiver newMessage'
]
