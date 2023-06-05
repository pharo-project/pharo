Extension { #name : #RPackageOrganizer }

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer class >> allManagers [

	^ MCWorkingCopy allManagers
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> allManagers [

	^ self class allManagers 
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> isDefinedAsPackageOrSubPackageInMC: aSymbol [
	"a category has been added. "
	
	^ self allManagers anySatisfy: [ :each | 
		self 
			category: each packageName asSymbol 
			matches: aSymbol ]
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> updateAfterNewMCPackageRegistred: anAnnouncement [
	"User create a new package, MCWorkingCopy propagates changes. We react accordingly."
	|  name |
	
	name := anAnnouncement package name.
	(self packageExactlyMatchingExtensionName: name) 
		ifNil: [ self ensureExistAndRegisterPackageNamed: name ]
]

{ #category : #'*Monticello-RPackage' }
RPackageOrganizer >> updateAfterNewMCPackageUnregistred: anAnnouncement [

]
