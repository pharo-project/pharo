Class {
	#name : #MetacelloMCProject,
	#superclass : #MetacelloProject,
	#category : #'Metacello-MC-Model'
}

{ #category : #scripting }
MetacelloMCProject >> asBaselineProject [
    ^ MetacelloMCBaselineProject new
]

{ #category : #scripting }
MetacelloMCProject >> asConfigurationProject [
    ^ self
]

{ #category : #'spec classes' }
MetacelloMCProject >> baselineOfProjectSpecClass [
    ^ MetacelloMCBaselineOfProjectSpec
]

{ #category : #'spec classes' }
MetacelloMCProject >> configurationOfProjectSpecClass [
    ^ MetacelloMCConfigurationOfProjectSpec
]

{ #category : #'repository creation' }
MetacelloMCProject >> createRepository: aRepositorySpec [

	^ MetacelloPlatform current createRepository: aRepositorySpec
]

{ #category : #'spec classes' }
MetacelloMCProject >> defaultLoaderClass [

	^MetacelloLoadingMCSpecLoader
]

{ #category : #'development support' }
MetacelloMCProject >> fetchProject [
	"fetch the latest version of the configuration package"
	
	^self fetchProject: MetacelloLoaderPolicy new
]

{ #category : #'development support' }
MetacelloMCProject >> fetchProject: aLoaderPolicy [
	"fetch the latest version of the configuration package"
	
	| mcLoader |
	(mcLoader := self loader) == nil
		ifTrue: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader doingLoads: [ 
		MCWorkingCopy
			managersForClass: self configuration class
			do: [:mgr | | pkg |
				pkg := self packageSpec.
				mgr repositoryGroup repositories do: [:repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
				pkg name: mgr packageName.
				pkg fetchUsing: mcLoader.
				^true ]].
	^true
]

{ #category : #'development support' }
MetacelloMCProject >> goferBranch: branchName project: commitMessage [
    | pkgSpec |
    pkgSpec := self projectPackage.
    pkgSpec file: pkgSpec name , '.' , branchName.
    ^ pkgSpec goferBranchPackage: branchName message: commitMessage
]

{ #category : #'development support' }
MetacelloMCProject >> goferCommitProject: commitMessage [

	| pkgSpec |
	(pkgSpec := self projectPackage) == nil ifTrue: [ ^false ].
	^pkgSpec goferCommitPackage: commitMessage
]

{ #category : #'spec classes' }
MetacelloMCProject >> packageSpec [

	^self packageSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloMCProject >> packageSpecClass [

	^MetacelloPackageSpec
]

{ #category : #'development support' }
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

{ #category : #private }
MetacelloMCProject >> pragmaKeywords [

	^super pragmaKeywords, #(projectPackage:attribute: packages:attribute: repositories:attribute: )
]

{ #category : #scripting }
MetacelloMCProject >> projectForScriptEngine: aMetacelloScriptEngine unconditionalLoad: aBool [
    ^ aMetacelloScriptEngine getConfigurationProjectUnconditionalLoad: aBool
]

{ #category : #'development support' }
MetacelloMCProject >> projectPackage [
  MCWorkingCopy
    managersForClass: self configuration class
    do: [ :mgr | 
      | pkgSpec repo |
      pkgSpec := self packageSpec
        name: mgr packageName;
        yourself.
      mgr ancestors notEmpty
        ifTrue: [ pkgSpec file: mgr ancestors first name ].
      repo := mgr repositoryGroup repositories
        detect: [ :each | each ~~ MetacelloPlatform current defaultPackageCache ]
        ifNone: [ 
          MetacelloNotification signal: ('Using cache repository for ' , self label , ' project package').
          MetacelloPlatform current defaultPackageCache ].
      pkgSpec repository: (repo asRepositorySpecFor: self).
      ^ pkgSpec ].
  ^ nil
]

{ #category : #'spec classes' }
MetacelloMCProject >> projectSpecClass [
	^ MetacelloMCNamelessProjectSpec
]

{ #category : #'spec classes' }
MetacelloMCProject >> repositoriesSpec [

	^self repositoriesSpecClass for: self
]

{ #category : #'spec classes' }
MetacelloMCProject >> repositoriesSpecClass [

	^MetacelloRepositoriesSpec
]

{ #category : #'spec classes' }
MetacelloMCProject >> repositorySpec [

	^self repositorySpecClass for: self
]

{ #category : #'spec classes' }
MetacelloMCProject >> repositorySpecClass [

	^MetacelloRepositorySpec
]

{ #category : #'development support' }
MetacelloMCProject >> saveProject [

	| pkgSpec |
	(pkgSpec := self projectPackage) == nil ifTrue: [ ^false ].
	^pkgSpec savePackage
]

{ #category : #'as yet unclassified' }
MetacelloMCProject >> setBaselineRepositoryDescription: aListOrRepositoryDescriptions [
    "noop "
]

{ #category : #'repository updating' }
MetacelloMCProject >> updatePackageRepositoriesFor: versionString [

	| versionSpec |
	(versionSpec := (self version: versionString) versionSpec) packageSpecsInLoadOrder do: [:pkgSpec |
		pkgSpec updatePackageRepositoriesFor: versionSpec ].
	^true
]

{ #category : #'development support' }
MetacelloMCProject >> updateProject [
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"
	
	^self updateProject: MetacelloLoaderPolicy new
]

{ #category : #'development support' }
MetacelloMCProject >> updateProject: aLoaderPolicy [
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"

	| mcLoader |
	(mcLoader := self loader) == nil
		ifTrue: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader
		doingLoads: [ 
			MCWorkingCopy
				managersForClass: self configuration class
				do: [ :mgr | 
					| pkg ar |
					pkg := self packageSpec.
					mgr repositoryGroup repositories do: [ :repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
					ar := mgr metacelloPackageNameWithBranch.
					pkg name: (ar at: 1).
					(ar at: 2) notEmpty
						ifTrue: [ pkg file: (ar at: 2) ].
					pkg load.
					^ true ] ].
	^ true
]

{ #category : #'spec classes' }
MetacelloMCProject >> versionSpecClass [

	^MetacelloMCVersionSpec
]
