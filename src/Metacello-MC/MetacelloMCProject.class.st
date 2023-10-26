Class {
	#name : 'MetacelloMCProject',
	#superclass : 'MetacelloProject',
	#category : 'Metacello-MC-Model',
	#package : 'Metacello-MC',
	#tag : 'Model'
}

{ #category : 'scripting' }
MetacelloMCProject >> asBaselineProject [
    ^ MetacelloMCBaselineProject new
]

{ #category : 'scripting' }
MetacelloMCProject >> asConfigurationProject [
    ^ self
]

{ #category : 'spec classes' }
MetacelloMCProject >> baselineOfProjectSpecClass [
    ^ MetacelloMCBaselineOfProjectSpec
]

{ #category : 'spec classes' }
MetacelloMCProject >> configurationOfProjectSpecClass [
    ^ MetacelloMCConfigurationOfProjectSpec
]

{ #category : 'repository creation' }
MetacelloMCProject >> createRepository: aRepositorySpec [

	^ MetacelloPlatform current createRepository: aRepositorySpec
]

{ #category : 'spec classes' }
MetacelloMCProject >> defaultLoaderClass [

	^MetacelloLoadingMCSpecLoader
]

{ #category : 'development support' }
MetacelloMCProject >> fetchProject [
	"fetch the latest version of the configuration package"
	
	^self fetchProject: MetacelloLoaderPolicy new
]

{ #category : 'development support' }
MetacelloMCProject >> fetchProject: aLoaderPolicy [
	"fetch the latest version of the configuration package"

	| mcLoader |
	(mcLoader := self loader) ifNil: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader doingLoads: [
		MCWorkingCopy workingCopiesForClass: self configuration class do: [ :workingCopy |
			| pkg |
			pkg := self packageSpec.
			workingCopy repositoryGroup repositories do: [ :repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
			pkg name: workingCopy packageName.
			pkg fetchUsing: mcLoader.
			^ true ] ].
	^ true
]

{ #category : 'development support' }
MetacelloMCProject >> goferBranch: branchName project: commitMessage [
    | pkgSpec |
    pkgSpec := self projectPackage.
    pkgSpec file: pkgSpec name , '.' , branchName.
    ^ pkgSpec goferBranchPackage: branchName message: commitMessage
]

{ #category : 'development support' }
MetacelloMCProject >> goferCommitProject: commitMessage [

	^ self projectPackage
		  ifNil: [ false ]
		  ifNotNil: [ :pkgSpec | pkgSpec goferCommitPackage: commitMessage ]
]

{ #category : 'spec classes' }
MetacelloMCProject >> packageSpec [

	^self packageSpecClass for: self
]

{ #category : 'spec classes' }
MetacelloMCProject >> packageSpecClass [

	^MetacelloPackageSpec
]

{ #category : 'development support' }
MetacelloMCProject >> packagesNeedSaving: aVersionString [
	"Answer a collection of associations (package -> repository) representing the packages 
	 reachable from this project that need to be saved"

	| packages |
	packages := Set new.
	(self version: aVersionString) spec 
		packagesNeedSavingVisited: (Set with: self configuration class name asString)
		into: packages.
	^packages
]

{ #category : 'private' }
MetacelloMCProject >> pragmaKeywords [

	^super pragmaKeywords, #(projectPackage:attribute: packages:attribute: repositories:attribute: )
]

{ #category : 'scripting' }
MetacelloMCProject >> projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: aBool [
    ^ aMetacelloScriptEngine getConfigurationProjectUnconditionalLoad: aBool
]

{ #category : 'development support' }
MetacelloMCProject >> projectPackage [
  MCWorkingCopy
    workingCopiesForClass: self configuration class
    do: [ :workingCopy | 
      | pkgSpec repo |
      pkgSpec := self packageSpec
        name: workingCopy packageName;
        yourself.
      workingCopy ancestors isNotEmpty
        ifTrue: [ pkgSpec file: workingCopy ancestors first name ].
      repo := workingCopy repositoryGroup repositories
        detect: [ :each | each ~~ MCCacheRepository default ]
        ifNone: [ 
          MetacelloNotification signal: ('Using cache repository for ' , self label , ' project package').
          MCCacheRepository default ].
      pkgSpec repository: (repo asRepositorySpecFor: self).
      ^ pkgSpec ].
  ^ nil
]

{ #category : 'spec classes' }
MetacelloMCProject >> projectSpecClass [
	^ MetacelloMCNamelessProjectSpec
]

{ #category : 'spec classes' }
MetacelloMCProject >> repositoriesSpec [

	^self repositoriesSpecClass for: self
]

{ #category : 'spec classes' }
MetacelloMCProject >> repositoriesSpecClass [

	^MetacelloRepositoriesSpec
]

{ #category : 'spec classes' }
MetacelloMCProject >> repositorySpec [

	^self repositorySpecClass for: self
]

{ #category : 'spec classes' }
MetacelloMCProject >> repositorySpecClass [

	^MetacelloRepositorySpec
]

{ #category : 'development support' }
MetacelloMCProject >> saveProject [

	^ self projectPackage
		  ifNil: [ false ]
		  ifNotNil: [ :pkgSpec | pkgSpec savePackage ]
]

{ #category : 'development support' }
MetacelloMCProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
    "noop "
]

{ #category : 'repository updating' }
MetacelloMCProject >> updatePackageRepositoriesFor: versionString [

	| versionSpec |
	(versionSpec := (self version: versionString) versionSpec) packageSpecsInLoadOrder do: [:pkgSpec |
		pkgSpec updatePackageRepositoriesFor: versionSpec ].
	^true
]

{ #category : 'development support' }
MetacelloMCProject >> updateProject [
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"
	
	^self updateProject: MetacelloLoaderPolicy new
]

{ #category : 'development support' }
MetacelloMCProject >> updateProject: aLoaderPolicy [
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"

	| mcLoader |
	(mcLoader := self loader) ifNil: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader doingLoads: [
		MCWorkingCopy workingCopiesForClass: self configuration class do: [ :workingCopy |
			| pkg ar |
			pkg := self packageSpec.
			workingCopy repositoryGroup repositories do: [ :repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
			ar := workingCopy metacelloPackageNameWithBranch.
			pkg name: (ar at: 1).
			(ar at: 2) isNotEmpty ifTrue: [ pkg file: (ar at: 2) ].
			pkg load.
			^ true ] ].
	^ true
]

{ #category : 'spec classes' }
MetacelloMCProject >> versionSpecClass [

	^MetacelloMCVersionSpec
]
