"
This class contains tests for class `AbstractFontProvider`
"
Class {
	#name : #AbstractFontProviderTest,
	#superclass : #TestCase,
	#category : #'Fonts-Infrastructure-Tests-Base'
}

{ #category : #tests }
AbstractFontProviderTest >> testIsAbstractClass [

	self
		assert: AbstractFontProvider isAbstract;
		assert: AbstractFontProvider hasSubclasses
]
