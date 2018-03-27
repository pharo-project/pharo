Class {
	#name : #MBCommandTests,
	#superclass : #MBAbstractTest,
	#instVars : [
		'gofer',
		'authorName',
		'tearDownPackages'
	],
	#category : #'Versionner-Tests-Core-Commands'
}

{ #category : #private }
MBCommandTests >> configurationName [

	^#'ConfigurationOfMetacello_Command_Tests'
]

{ #category : #running }
MBCommandTests >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #private }
MBCommandTests >> repositoryName [

	^#'Metacello_Dev_Cycle_Repository'
]

{ #category : #running }
MBCommandTests >> runCase [
	| original |
	original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
	^ [ 	MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
		[ super runCase ]
			ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ] ]
]

{ #category : #running }
MBCommandTests >> setUp [

	| repo |
	super setUp.
	gofer := Gofer new.
	gofer disablePackageCache.
	repo := MBMonticelloPackagesResource current monticelloRepository.
	gofer repository: repo.
	Smalltalk at: #'MB_Foo_Test_Repository' put: repo.

	repo := MCDictionaryRepository new.
	repo description: 'dictionary://', self repositoryName asString.
	Smalltalk at: self repositoryName put: repo.

	authorName := MetacelloPlatform current authorName.
	MetacelloPlatform current authorName: 'TesterBob'.

]

{ #category : #running }
MBCommandTests >> tearDown [
	gofer := Gofer new.
	self tearDownPackages do: [:pkgName | (self hasPackage: pkgName) ifTrue: [ gofer package: pkgName ]].
	gofer references notEmpty ifTrue: [ gofer unload ].

	Smalltalk removeKey: self repositoryName ifAbsent: [  ].
	Smalltalk removeKey: #'MB_Foo_Test_Repository' ifAbsent: [].
	
	Smalltalk at: self configurationName ifPresent: [:cls | cls removeFromSystem ].

	MetacelloPlatform current authorName: authorName.
	super tearDown
]

{ #category : #accessing }
MBCommandTests >> tearDownPackages [

	tearDownPackages ifNil: [ tearDownPackages := OrderedCollection new ].
	^tearDownPackages
]
