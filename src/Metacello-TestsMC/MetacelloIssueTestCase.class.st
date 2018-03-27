Class {
	#name : #MetacelloIssueTestCase,
	#superclass : #TestCase,
	#instVars : [
		'gofer',
		'initialWorkingCopyList'
	],
	#category : #'Metacello-TestsMC'
}

{ #category : #running }
MetacelloIssueTestCase >> configurationRepository [
	"MetacelloConfigurationResource reset"

	^ MetacelloConfigurationResource current monticelloRepository
]

{ #category : #utilities }
MetacelloIssueTestCase >> hasPackage: aString [
	| package |
	package := MCWorkingCopy allManagers
		detect: [ :each | each packageName = aString ]
		ifNone: [ nil ].
	^ package notNil
]

{ #category : #accessing }
MetacelloIssueTestCase >> monticelloRepository [
	"MetacelloMonticelloResource reset"
	^ MetacelloMonticelloResource current monticelloRepository
]

{ #category : #accessing }
MetacelloIssueTestCase >> project [

	^self projectWith: #()
]

{ #category : #accessing }
MetacelloIssueTestCase >> projectClass [
    ^ Smalltalk at: self projectName asSymbol
]

{ #category : #accessing }
MetacelloIssueTestCase >> projectName [
    ^ self subclassResponsibility
]

{ #category : #accessing }
MetacelloIssueTestCase >> projectWith: projectAttributes [
    | project |
    "Construct Metacello project"
    project := MetacelloMCProject new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self projectClass new project: project.
    project loader: MetacelloNullRecordingMCSpecLoader new.
    project loader evalDoits: false.
    ^ project
]

{ #category : #running }
MetacelloIssueTestCase >> runCase [
	| original |
	original := MetacelloPlatform current bypassGoferLoadUpdateCategories.
	[ 
	MetacelloPlatform current bypassGoferLoadUpdateCategories: true.
	^ MetacelloPlatform current suspendSystemUpdateEventsDuring: [ super runCase ] ]
		ensure: [ MetacelloPlatform current bypassGoferLoadUpdateCategories: original ]
]

{ #category : #running }
MetacelloIssueTestCase >> setUp [
  | repo |
  super setUp.
  MetacelloPlatform current clearPackageCache.
  repo := self monticelloRepository.
  gofer := Gofer new.
  gofer disablePackageCache.
  gofer repository: repo.
  Smalltalk at: #'Metacello_Gofer_Test_Repository' put: repo.
  repo := self configurationRepository.
  gofer repository: repo.
  Smalltalk at: #'Metacello_Configuration_Test_Repository' put: repo.
  initialWorkingCopyList := MCWorkingCopy allManagers
    collect: [ :each | each packageName ]
]

{ #category : #running }
MetacelloIssueTestCase >> tearDown [
  | aGofer finalWorkingCopyList diff |
  aGofer := Gofer new.
  self tearDownPackageNames
    do: [ :pName | 
      (self hasPackage: pName)
        ifTrue: [ aGofer package: pName ] ].
  aGofer references notEmpty
    ifTrue: [ aGofer metacelloUnload ].
  MCRepositoryGroup default
    removeIdenticalRepository:
        (Smalltalk globals at: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ]);
    removeIdenticalRepository:
        (Smalltalk globals at: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ]);
    yourself.
  Smalltalk globals removeKey: #'Metacello_Gofer_Test_Repository' ifAbsent: [  ].
  Smalltalk globals removeKey: #'Metacello_Configuration_Test_Repository' ifAbsent: [  ].
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
MetacelloIssueTestCase >> tearDownPackageNames [
    ^ {(self projectName)}
]
