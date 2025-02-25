"
This class contains tests for class `AbstractFontProvider`
"
Class {
	#name : 'AbstractFontProviderTest',
	#superclass : 'TestCase',
	#category : 'Fonts-Infrastructure-Tests-Base',
	#package : 'Fonts-Infrastructure-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
AbstractFontProviderTest >> testIsAbstract [

	self
		assert: AbstractFontProvider isAbstract;
		assert: AbstractFontProvider hasSubclasses
]
