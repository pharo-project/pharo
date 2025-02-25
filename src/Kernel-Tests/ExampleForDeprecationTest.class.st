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

	self
		deprecated: 'This method is part of the DeprecationTest.'
		transformWith: '`@receiver deprecatedMessageWithTransform' -> '`@receiver newMessage'
]
