"
An EmbeddedFreeTypeFontInstallerTest is a test class for testing the behavior of EmbeddedFreeTypeFontInstaller
"
Class {
	#name : #EmbeddedFreeTypeFontInstallerTest,
	#superclass : #TestCase,
	#category : #'EmbeddedFreeType-Tests'
}

{ #category : #accessing }
EmbeddedFreeTypeFontInstallerTest >> currentInstallerOrError [
	| installer |
	installer := EmbeddedFreeTypeFontInstaller readSlotNamed: #current.
	self assert: installer notNil.
	^ installer
]

{ #category : #accessing }
EmbeddedFreeTypeFontInstallerTest >> currentProviderOrError [
	| provider |
	provider := FreeTypeFontProvider readSlotNamed: #current.
	self assert: provider notNil.
	^ provider
]

{ #category : #tests }
EmbeddedFreeTypeFontInstallerTest >> testIsRegistred [
	| installer provider |
	installer := self currentInstallerOrError.
	provider := self currentProviderOrError.
	self assert: (provider includesInstaller: installer)
]

{ #category : #tests }
EmbeddedFreeTypeFontInstallerTest >> testinstallAllFontsIn [

	| provider fileInfos families |
	provider := self currentProviderOrError 
		updateEmbeddedFreeTypeFonts;
		yourself.
	fileInfos := provider fileInfosByFamilyAndGroup.
	self assert: fileInfos keys size > 1.
	self assert: (fileInfos includesKey: 'Source Sans Pro').
	self assert: (fileInfos includesKey: 'Source Code Pro').
	families := provider families.
	self assert: (families includesKey: 'Source Sans Pro').
	self assert: (families includesKey: 'Source Code Pro').

]
