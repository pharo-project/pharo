Extension { #name : #Collection }

{ #category : #'*metacello-core' }
Collection >> addToMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each addToMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : #'*metacello-core' }
Collection >> asMetacelloAttributeList [
    ^ self
]

{ #category : #'*metacello-core' }
Collection >> asMetacelloAttributePath [
    ^ MetacelloMethodSectionPath withAll: self
]

{ #category : #'*metacello-core-scripting' }
Collection >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeCollection: self do: projectSpecBlock
]

{ #category : #'*metacello-core' }
Collection >> mergeIntoMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each mergeIntoMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : #'*metacello-core' }
Collection >> removeFromMetacelloPackages: aMetacelloPackagesSpec [

	self do: [:each | each removeFromMetacelloPackages: aMetacelloPackagesSpec ]
]

{ #category : #'*metacello-core' }
Collection >> setForDo: aBlock withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: self do: aBlock
]

{ #category : #'*metacello-core' }
Collection >> setForVersion: aString withInMetacelloConfig: aMetacelloConstructore [
    aMetacelloConstructore setFor: self version: aString
]

{ #category : #'*metacello-core' }
Collection >> setImportInVersionSpec: aMetacelloVersionSpec [
  aMetacelloVersionSpec setImport: self asArray
]

{ #category : #'*metacello-core' }
Collection >> setIncludesInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setIncludes: self asArray.
]

{ #category : #'*metacello-core' }
Collection >> setLoadsInMetacelloProject: aMetacelloPackageSpec [

	aMetacelloPackageSpec setLoads: self asArray.
]

{ #category : #'*metacello-core' }
Collection >> setRequiresInMetacelloPackage: aMetacelloPackageSpec [

	aMetacelloPackageSpec setRequires: self asArray.
]
