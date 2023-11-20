Extension { #name : 'Gofer' }

{ #category : '*Gofer-Deprecated' }
Gofer >> cleanup [
	"Cleans the specified packages."

	^ self execute: GoferCleanup
]

{ #category : '*Gofer-Deprecated' }
Gofer >> commit [
	"Commit the modified packages."

	^ self execute: GoferCommit
]

{ #category : '*Gofer-Deprecated' }
Gofer >> commit: aString [
	"Commit the modified packages with the given commit message."

	^ self execute: GoferCommit do: [ :operation | operation message: aString ]
]

{ #category : '*Gofer-Deprecated' }
Gofer >> fetch [
	"Download versions from remote repositories into the local cache."

	^ self execute: GoferFetch
]

{ #category : '*Gofer-Deprecated' }
Gofer >> loadBleedingEdge [
	"Load the baseline of the previously specifed configuration."

	^ self loadVersion: #bleedingEdge
]

{ #category : '*Gofer-Deprecated' }
Gofer >> loadDevelopment [
	"Load the development version of the previously specifed configuration."

	^ self loadVersion: #development
]

{ #category : '*Gofer-Deprecated' }
Gofer >> loadStable [
	"Load the stable version of the previously specifed configuration."

	^ self loadVersion: #stable
]

{ #category : '*Gofer-Deprecated' }
Gofer >> loadVersion: anMetacelloVersionIdentifierSymbol [
	"Load the given version name"
	^ self execute: GoferMetacelloLoad do: [ :operation|
		operation version: anMetacelloVersionIdentifierSymbol ]
]

{ #category : '*Gofer-Deprecated' }
Gofer >> localChanges [
	"Answer the changes between the base version and the working copy."

	^ self execute: GoferLocalChanges
]

{ #category : '*Gofer-Deprecated' }
Gofer >> merge [
	"Merge the specified packages into their working copies."

	^ self execute: GoferMerge
]

{ #category : '*Gofer-Deprecated' }
Gofer >> package: aString constraint: aOneArgumentBlock [
	"Add the package aString to the receiver, but constraint the resulting versions further with aOneArgumentBlock. For details on the package, see #package:. The following example defines various constraints:

		aGofer package: 'Magritte-Seaside' constraint: [ :version |
			version author = 'lr'
				and: [ version branch = 'trial'
				and: [ version versionNumber > 120 ] ] ]"

	references addLast: (GoferConstraintReference name: aString constraint: aOneArgumentBlock)
]

{ #category : '*Gofer-Deprecated' }
Gofer >> push [
	"Upload local versions from local cache into remote repositories."

	^ self execute: GoferPush
]

{ #category : '*Gofer-Deprecated' }
Gofer >> recompile [
	"Recompile the specified packages."

	^ self execute: GoferRecompile
]

{ #category : '*Gofer-Deprecated' }
Gofer >> reinitialize [
	"Calls the class side initializers on all package code."

	^ self execute: GoferReinitialize
]

{ #category : '*Gofer-Deprecated' }
Gofer >> remoteChanges [
	"Display the changes between the working copy and the remote changes."

	^ self execute: GoferRemoteChanges
]

{ #category : '*Gofer-Deprecated' }
Gofer >> revert [
	"Revert the specified packages to the currently loaded version."

	^ self execute: GoferRevert
]

{ #category : '*Gofer-Deprecated' }
Gofer >> unload [
	"Unload the specified packages."

	^ self execute: GoferUnload
]

{ #category : '*Gofer-Deprecated' }
Gofer >> update [
	"Update the specified packages."

	^ self execute: GoferUpdate
]
