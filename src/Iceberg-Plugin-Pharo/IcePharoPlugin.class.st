Class {
	#name : #IcePharoPlugin,
	#superclass : #IcePlugin,
	#category : #'Iceberg-Plugin-Pharo-Core'
}

{ #category : #events }
IcePharoPlugin class >> addPharoProjectToIceberg [
	"It will take all packages corresponding to Pharo project and it will create an iceberg 
	 project with it"
	| packageNames repository commit workingCopy |
	
	self flag: #TODO. "Refactor into something better (a class and a hierarchy, so we can add 
	other packages)"
	
	"extract package names from baselines"
	packageNames := (#(
		BaselineOfAthens
		BaselineOfBasicTools
		BaselineOfDisplay
		BaselineOfFlashback
		BaselineOfIDE
		BaselineOfMonticello
		BaselineOfMenuRegistration
		BaselineOfMetacello
		BaselineOfMorphic
		BaselineOfMorphicCore 
		BaselineOfPharo
		BaselineOfPharoBootstrap
		BaselineOfSUnit
		BaselineOfShift 
		BaselineOfSpec
		BaselineOfTraits
		BaselineOfUI    
		BaselineOfUnifiedFFI ) 
		collect: [ :each | (Smalltalk globals at: each) allPackageNames copyWith: each ]) 
		flattened 
		asSet.
		
	repository := IceLibgitRepository new
		name: 'pharo';
		yourself.
	commit := (IceUnknownCommit new 
			id: SystemVersion current commitHash;
			repository: repository;
 			yourself).
	workingCopy := IceWorkingCopy basicNew
		repository: repository;
		initialize;
		referenceCommit: commit;
		project: ((IceBasicProject onRepository: repository)
			sourceDirectory: 'src';
			yourself);
		yourself.
	repository workingCopy: workingCopy.
		
	commit repository: repository.
	"add packages to project"
	packageNames
		do: [ :each |
			repository workingCopy basicAddPackage: (IcePackage
				named: each
				repository: repository) ].
	"register project"
	repository register
]

{ #category : #testing }
IcePharoPlugin class >> isAvailableFor: aRepository [

	^ aRepository remotes anySatisfy: [ :each | each projectName = 'pharo' ]
]

{ #category : #events }
IcePharoPlugin >> fetchIfNeeded: aRepository [

	(aRepository lookupCommit: SystemVersion current commitHash) isUnknownCommit
		ifFalse: [ ^ self ].

	UIManager default
		informUser: 'Fetching incoming commits'
		during: [ aRepository remotes
				detect: [ :each | each projectName = 'pharo-project' ]
				ifFound: [ :each | each fetch ] ]
]

{ #category : #events }
IcePharoPlugin >> repositoryWasCreated: aRepository [

]

{ #category : #events }
IcePharoPlugin >> repositoryWillBeCreated: aRepository [
	SystemVersion current major < 7 ifTrue: [ ^ self ].
	
	self fetchIfNeeded: aRepository.
	(aRepository lookupCommit: SystemVersion current commitHash) adopt.
]
