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
MetacelloAbstractPackageSpec >> resolveToAllPackagesIn: aVersionSpec into: packages visited: visited [

	visited
		visit: self
		doing: [:spec |
			self visitingWithPackages: packages.
			(spec includes, spec requires) do: [:pkgName |
				(aVersionSpec packageNamed: pkgName)
					projectDo: [:prj | 
						(prj resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:pkg | 
							packages at: pkg name put: pkg ]] 
					packageDo: [:pkg | 
						packages at: pkg name put: pkg.
						(pkg resolveToAllPackagesIn: aVersionSpec visited: visited) do: [:rpkg | 
							packages at: rpkg name put: rpkg ] ] 
					groupDo: [:grp | grp resolveToAllPackagesIn: aVersionSpec into: packages  visited: visited ]]]
]

{ #category : '*Metacello-MC' }
MetacelloAbstractPackageSpec >> resolveToAllPackagesIn: aVersionSpec visited: visited [

	| packages |
	packages := Dictionary new.
	self resolveToAllPackagesIn: aVersionSpec into: packages visited: visited.
	^packages values asOrderedCollection
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
