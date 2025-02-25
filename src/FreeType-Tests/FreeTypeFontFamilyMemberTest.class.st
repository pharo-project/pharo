"
This class contains tests for class `FreeTypeFontFamilyMember`
"
Class {
	#name : 'FreeTypeFontFamilyMemberTest',
	#superclass : 'TestCase',
	#category : 'FreeType-Tests-FontManager',
	#package : 'FreeType-Tests',
	#tag : 'FontManager'
}

{ #category : 'tests' }
FreeTypeFontFamilyMemberTest >> testBeSimulatedStyle [

	| member |
	member := FreeTypeFontFamilyMember new.
	self deny: member isSimulatedStyle.
	member beSimulatedStyle.
	self assert: member isSimulatedStyle
]

{ #category : 'tests' }
FreeTypeFontFamilyMemberTest >> testIsNotSimulatedStyleByDefault [

	self deny: FreeTypeFontFamilyMember new isSimulatedStyle
]
