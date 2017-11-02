"
A VersionnerRepositoriesManager allows to select / deselect  / add new repositories. The UI is shared with Komitter.
"
Class {
	#name : #VersionnerRepositoriesManager,
	#superclass : #Object,
	#instVars : [
		'remotesUI',
		'packageName'
	],
	#category : 'Versionner-Spec-Browser'
}

{ #category : #'instance creation' }
VersionnerRepositoriesManager class >> newWithPackage: aPackageName [
	| repoManager |
	
	repoManager := self new.
	aPackageName ifNotNil: [ repoManager 
								packageName: aPackageName;
								initializeSelectedRemotes;
								yourself ].
	^ repoManager
]

{ #category : #initialization }
VersionnerRepositoriesManager >> initialize [

	super initialize.
	remotesUI := RemotesManager new.
]

{ #category : #initialization }
VersionnerRepositoriesManager >> initializeSelectedRemotes [
	| workingCopy |
	
	workingCopy := MCWorkingCopy allManagers detect: [ :wc | wc package name = packageName ].
	remotesUI selectedRemotes: workingCopy repositoryGroup repositories asOrderedCollection.
	remotesUI initializeWidgets.
	remotesUI initializePresenter

]

{ #category : #ui }
VersionnerRepositoriesManager >> open [
	| dialog |
	dialog := remotesUI openDialogWithSpec.
	dialog okAction: [  ]
]

{ #category : #accessing }
VersionnerRepositoriesManager >> packageName: aString [
	packageName := aString
]

{ #category : #protocol }
VersionnerRepositoriesManager >> selectedChangedBlock: aBlock [
	remotesUI selectedChangedBlock: aBlock
]
