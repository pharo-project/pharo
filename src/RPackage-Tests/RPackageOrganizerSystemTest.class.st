"
I am a test case to make sure the Smalltalk environment work as intended with its system package organizer.
"
Class {
	#name : #RPackageOrganizerSystemTest,
	#superclass : #TestCase,
	#category : #'RPackage-Tests-Image'
}

{ #category : #tests }
RPackageOrganizerSystemTest >> testDefault [

	self assert: Smalltalk organization identicalTo: self class environment organization
]

{ #category : #tests }
RPackageOrganizerSystemTest >> testDefaultEnvironment [
	"When no environment is specified, point to the default one"

	self assert: RPackageOrganizer new environment identicalTo: Smalltalk globals
]

{ #category : #tests }
RPackageOrganizerSystemTest >> testThatExistingPackageNamesDoesNotContainIllegalCharacters [
	| illegalCharacters |

	illegalCharacters := #( $\ $/ $: $* $? $" $< $> $| ).

	self class environment organization categories do: [ :aPackageName |
		self deny: (aPackageName includesAnyOf: illegalCharacters) ]
]
