Extension { #name : 'Collection' }

{ #category : '*Metacello-Core' }
Collection >> addToMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each addToMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core-scripting' }
Collection >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeCollection: self do: projectSpecBlock
]

{ #category : '*Metacello-Core' }
Collection >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each mergeIntoMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each removeFromMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : '*Metacello-Core' }
Collection >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: self asArray.
]

{ #category : '*Metacello-Core' }
Collection >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: self asArray.
]

{ #category : '*Metacello-Core' }
Collection >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: self asArray.
]
