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

	self assert: RPackageOrganizer new environment identicalTo: Smalltalk globals
]

{ #category : 'tests' }
RPackageOrganizerSystemTest >> testThatExistingPackageNamesDoesNotContainIllegalCharacters [
	| illegalCharacters |

	illegalCharacters := #( $\ $/ $: $* $? $" $< $> $| ).

	self packageOrganizer categories do: [ :aPackageName |
		self deny: (aPackageName includesAnyOf: illegalCharacters) ]
]
