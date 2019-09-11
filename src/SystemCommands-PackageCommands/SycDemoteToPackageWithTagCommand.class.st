"
I am a command to demote given packages into the package with tag
"
Class {
	#name : #SycDemoteToPackageWithTagCommand,
	#superclass : #SycPackageCommand,
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #execution }
SycDemoteToPackageWithTagCommand >> applyResultInContext: aBrowserContext [
			
	| lastPackage newPackage tag |
	lastPackage := packages detect: [ :each | each name includes: $- ] ifNone: [ ^self ].

	newPackage := (lastPackage name copyUpToLast: $-) asPackage.
	tag := lastPackage name last: lastPackage name size - newPackage name size - 1.
	aBrowserContext showPackage: newPackage atClassTag: tag asSymbol
]

{ #category : #accessing }
SycDemoteToPackageWithTagCommand >> defaultMenuItemName [
	^'Demote to package with tag'
]

{ #category : #execution }
SycDemoteToPackageWithTagCommand >> execute [
			
	packages 
		select: [ :each | each name includes: $- ]
		thenDo: [ :each | each demoteToTagInPackageNamed: (each name copyUpToLast: $-) ]
]
