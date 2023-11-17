"
I am a test case to make sure the Smalltalk environment work as intended with its system package organizer.
"
Class {
	#name : 'RPackageOrganizerSystemTest',
	#superclass : 'TestCase',
	#category : 'RPackage-Tests-Image',
	#package : 'RPackage-Tests',
	#tag : 'Image'
}

{ #category : 'tests' }
RPackageOrganizerSystemTest >> testDefault [

	self assert: self packageOrganizer identicalTo: self class environment organization
]

{ #category : 'tests' }
RPackageOrganizerSystemTest >> testDefaultEnvironment [
	"When no environment is specified, point to the default one"

	self assert: PackageOrganizer new environment identicalTo: Smalltalk globals
]

{ #category : 'tests' }
RPackageOrganizerSystemTest >> testThatExistingPackageNamesDoesNotContainIllegalCharacters [

	| illegalCharacters |
	illegalCharacters := #( $\ $/ $: $* $? $" $< $> $| ).

	self packageOrganizer packages do: [ :package |
		self deny: (package name includesAnyOf: illegalCharacters).
		package classTags do: [ :tag | self deny: (tag name includesAnyOf: illegalCharacters) ] ]
]
