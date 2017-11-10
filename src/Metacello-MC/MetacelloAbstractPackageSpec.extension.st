Extension { #name : #MetacelloAbstractPackageSpec }

{ #category : #'*metacello-mc-querying' }
MetacelloAbstractPackageSpec >> file [ 
	"MetacelloPackageSpec compatibility"
	
	^nil
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> forceUpdatePackageSpec: updatedSpecs using: anMCLoader [

	^self updatePackageSpec: updatedSpecs using: anMCLoader
]

{ #category : #'*metacello-mc-querying' }
MetacelloAbstractPackageSpec >> getFile [
  "MetacelloPackageSpec compatibility"

  ^ nil
]

{ #category : #'*metacello-mc-querying' }
MetacelloAbstractPackageSpec >> isPackageLoaded [

	^false
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> loadUsing: aLoader gofer: gofer [

	^self subclassResponsibility
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> packagesNeedSavingVisited: visitedProjects using: repos into: aCollection [
	"noop by default"
]

{ #category : #'*metacello-mc-querying' }
MetacelloAbstractPackageSpec >> repository [
    self deprecated: 'Use repositories or repositorySpecs'.
    ^ nil
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> repositorySpecs [

	^#()
]

{ #category : #'*metacello-mc' }
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

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> resolveToAllPackagesIn: aVersionSpec visited: visited [

	| packages |
	packages := Dictionary new.
	self resolveToAllPackagesIn: aVersionSpec into: packages visited: visited.
	^packages values asOrderedCollection
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> resolveToLoadableSpec [

	^self
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited [
  ^ self resolveToPackagesIn: aVersionSpec visited: visited
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	^self subclassResponsibility
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> updateForSpawnMethod: sourceSpec [
	"This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

	answers := name := requires := includes := nil
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> updatePackageRepositoriesFor: aVersionSpec [
	"noop by default"
	
	^true
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> updatePackageSpec: updatedSpecs using: anMCLoader [
	"Add pkg copy to updatedSpecs if the file in current image is different from the receiver's file"
]

{ #category : #'*metacello-mc-querying' }
MetacelloAbstractPackageSpec >> version [ 
	"MetacelloPackageSpec compatibility"
	
	^nil
]

{ #category : #'*metacello-mc' }
MetacelloAbstractPackageSpec >> visitingWithPackages: packages [
	"noop"
]
