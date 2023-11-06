Extension { #name : 'MetacelloProjectReferenceSpec' }

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> loadUsing: aLoader gofer: ignored [

	| required |
	required := self resolveToLoadableSpec.
	required loader: aLoader.
	^required load
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> packagesNeedSavingVisited: visitedProjects using: repos into: aCollection [

	| prjct clsName vrsn |
	prjct := self resolveToLoadableSpec.
	(visitedProjects includes: (clsName := prjct className)) ifTrue: [ ^ self ].
	visitedProjects add: clsName.
	(vrsn := self versionOrNil) ifNil: [ ^ self ].
	vrsn spec packagesNeedSavingVisited: visitedProjects into: aCollection
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> repository: aStringOrMetacelloRepositorySpec [
    ^ self projectReference repository: aStringOrMetacelloRepositorySpec
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveProjectSpec [

	^self projectReference
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToAllPackagesIn: aVersionSpec into: packages visited: visited [

	(self resolveProjectSpec resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:pkg |
		packages at: pkg put: pkg ]
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToLoadableSpec [

	^self resolveProjectSpec resolveToLoadableSpec
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited [
  ^ andProjectsBool
    ifTrue: [ {self} ]
    ifFalse: [ self resolveToPackagesIn: aVersionSpec visited: visited ]
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^#()
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

	| nm |
	nm := name.
	self projectReference updateForSpawnMethod: sourceSpec.
	super updateForSpawnMethod: sourceSpec.
	name := nm.
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> updatePackageSpec: updatedSpecs using: anMCLoader [
	"Add project copy to updatedSpecs if the current version of the project 
	 is different from the receiver's version"
	
	self projectReference updatePackageSpec: updatedSpecs
]

{ #category : '*Metacello-MC' }
MetacelloProjectReferenceSpec >> versionString [

	^self projectReference versionString
]
