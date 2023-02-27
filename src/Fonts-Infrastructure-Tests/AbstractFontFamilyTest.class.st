"
This class contains tests for class `AbstractFontFamily`
"
Class {
	#name : #AbstractFontFamilyTest,
	#superclass : #TestCase,
	#category : #'Fonts-Infrastructure-Tests-Base'
}

{ #category : #tests }
AbstractFontFamilyTest >> testIsAbstractClass [

	self
		assert: AbstractFontFamily isAbstract;
		assert: AbstractFontFamily hasSubclasses
]
