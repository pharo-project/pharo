Extension { #name : 'MetacelloAbstractPackageSpec' }

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> file [ 
	"MetacelloPackageSpec compatibility"
	
	^nil
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> getFile [
  "MetacelloPackageSpec compatibility"

  ^ nil
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> isPackageLoaded [

	^false
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> loadUsing: aLoader gofer: gofer [

	^self subclassResponsibility
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> packagesNeedSavingVisited: visitedProjects using: repos into: aCollection [
	"noop by default"
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> repositorySpecs [

	^#()
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> resolveToLoadableSpec [

	^self
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited [
  ^ self resolveToPackagesIn: aVersionSpec visited: visited
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^self subclassResponsibility
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> version [ 
	"MetacelloPackageSpec compatibility"
	
	^nil
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> visitingWithPackages: packages [
	"noop"
]
