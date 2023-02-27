"
This class contains tests for class `AbstractFontFamilyMember`
"
Class {
	#name : #AbstractFontFamilyMemberTest,
	#superclass : #TestCase,
	#category : #'Fonts-Infrastructure-Tests-Base'
}

{ #category : #tests }
AbstractFontFamilyMemberTest >> testIsAbstractClass [

	self
		assert: AbstractFontFamilyMember isAbstract;
		assert: AbstractFontFamilyMember hasSubclasses
]
