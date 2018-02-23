Class {
	#name : #MetacelloDictionaryRepositoryTest,
	#superclass : #GoferTest,
	#instVars : [
		'tempRepositories',
		'initialWorkingCopyList',
		'undefinedSymbols'
	],
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloDictionaryRepositoryTest >> alternateRepository [
	"MetacelloAlternateResource reset"

	^ MetacelloAlternateResource current monticelloRepository
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> configurationRepository [
	"MetacelloConfigurationResource reset"

	^ MetacelloConfigurationResource current monticelloRepository
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> defaultTimeout [
	"I don't want no stkinkin' timeouts"
	^60000
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> doSilently [

	^true
]

{ #category : #utilities }
MetacelloDictionaryRepositoryTest >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #accessing }
MetacelloDictionaryRepositoryTest >> loadType [

	^#linear
]

{ #category : #accessing }
MetacelloDictionaryRepositoryTest >> monticelloRepository [
	"MetacelloMonticelloResource reset"
	^ MetacelloMonticelloResource current monticelloRepository
]

{ #category : #accessing }
MetacelloDictionaryRepositoryTest >> project [

	| constructor project |
	"Construct Metacello project"
	constructor := MetacelloVersionConstructor on: self.
	project := constructor project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: self loadType.
	^project
]

{ #category : #accessing }
MetacelloDictionaryRepositoryTest >> projectWith: projectAttributes [

	| project |
	"Construct Metacello project"
	project := MetacelloMCProject new.
	project projectAttributes: projectAttributes.
	MetacelloVersionConstructor on: self project: project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: self loadType.
	^project

]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> runCase [
	| original |
	(self doSilently) ifFalse: [ ^super runCase ].
	original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
	[ 
	MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
	^ MetacelloPlatform current suspendSystemUpdateEventsDuring: [ super runCase ] ]
		ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ]
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> setUp [
  | repo |
  super setUp.
  MetacelloPlatform current clearPackageCache.
  MetacelloConfigurationResource projectAttributes: nil.
  repo := self monticelloRepository.
  self tempRepositories add: repo.
  gofer repository: repo.
  Smalltalk at: #'Metacello_Gofer_Test_Repository' put: repo.
  repo := self alternateRepository.
  self tempRepositories add: repo.
  Smalltalk at: #'Metacello_Configuration_Test_Alternate_Repository' put: repo.
  repo := self configurationRepository.
  self tempRepositories add: repo.
  Smalltalk at: #'Metacello_Configuration_Test_Repository' put: repo.
  initialWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  undefinedSymbols := MetacelloPlatform current disableUndefinedSybolUpdates
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> tearDown [
  | aGofer finalWorkingCopyList diff |
  aGofer := Gofer new.
  self tearDownPackages: aGofer.
  aGofer references notEmpty
    ifTrue: [ aGofer metacelloUnload ].
  Smalltalk globals removeKey: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ].
  Smalltalk globals removeKey: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ].
  Smalltalk globals
    removeKey: #'Metacello_Configuration_Test_Alternate_Repository'
    ifAbsent: [  ].
  self tempRepositories
    do: [ :repo | MCRepositoryGroup default removeIdenticalRepository: repo ].
  MetacelloPlatform current reenableUndefinedSybolUpdates: undefinedSymbols.
  finalWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ].
  diff := finalWorkingCopyList difference: initialWorkingCopyList.
  diff
    do: [ :leak | 
      Transcript
        cr;
        show:
            'leaked package from ' , self printString , ' -> ' , leak printString ].
  self assert: diff isEmpty.
  super tearDown
]

{ #category : #running }
MetacelloDictionaryRepositoryTest >> tearDownPackages: aGoferInstance [

	(self hasPackage: 'GoferFoo')
		ifTrue: [ aGoferInstance package: 'GoferFoo' ].
	(self hasPackage: 'GoferBar')
		ifTrue: [ aGoferInstance package: 'GoferBar' ].
	(self hasPackage: 'GoferFaux')
		ifTrue: [ aGoferInstance package: 'GoferFaux' ].
	(self hasPackage: 'GoferBeau')
		ifTrue: [ aGoferInstance package: 'GoferBeau' ].
	(self hasPackage: 'MetacelloTestConfigurationOfFoo')
		ifTrue: [ aGoferInstance package: 'MetacelloTestConfigurationOfFoo' ].

]

{ #category : #accessing }
MetacelloDictionaryRepositoryTest >> tempRepositories [

	tempRepositories ifNil: [ tempRepositories := OrderedCollection new ].
	^tempRepositories
]
