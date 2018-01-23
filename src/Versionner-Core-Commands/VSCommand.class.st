"
VSCommand is a command design pattern for capturing actions.
"
Class {
	#name : #VSCommand,
	#superclass : #Object,
	#instVars : [
		'target',
		'requestor',
		'workflow'
	],
	#category : #'Versionner-Core-Commands'
}

{ #category : #'instance creation' }
VSCommand class >> target: target for: requestor [
	^ self new
		target: target;
		requestor: requestor;
		yourself
]

{ #category : #testing }
VSCommand >> canBeApplied [
	^ true
]

{ #category : #repositories }
VSCommand >> chooseRepositoryFromList: repositories [
	repositories size > 1
		ifTrue: [ 
			^ UIManager default
				chooseFrom: (repositories collect: [ :repository | repository description ])
				values: repositories
				title: 'Select repository' ].
	^ repositories first
]

{ #category : #execute }
VSCommand >> execute [
	self subclassResponsibility 
]

{ #category : #execute }
VSCommand >> executeWithDevWorkflow: devWorkflow [
	workflow := devWorkflow.
	self execute
]

{ #category : #ordering }
VSCommand >> order [
	^ self title
]

{ #category : #repositories }
VSCommand >> repositoryOfPackageNamed: packageName [
	"Return the repository of the package given as parameter. Create one (using UI) if necessary"

	| mcPackage workingCopy repositories repository repositoryIndex |
	mcPackage := MCPackage named: packageName.
	workingCopy := mcPackage workingCopy.
	repositories := workingCopy repositoryGroup repositories reject: [ :rep | rep == MCCacheRepository uniqueInstance ].
	repositories
		ifEmpty: [ repository := MCTool newRepository.
				   workingCopy repositoryGroup addRepository: repository. ].
	repositories size > 1
		ifTrue: [ 
			repositoryIndex := self uiManager chooseFrom: (repositories collect: #printString).
			repositoryIndex = 0
				ifTrue: [ ^ nil ].
			repository := repositories at: repositoryIndex ].
	repositories size = 1
		ifTrue: [ repository := repositories first ].
	^ repository
]

{ #category : #accessing }
VSCommand >> requestor [
	^ requestor
]

{ #category : #accessing }
VSCommand >> requestor: anObject [
	requestor := anObject
]

{ #category : #ui }
VSCommand >> showExecuteWhile: aBlock [

	^Cursor execute showWhile:  aBlock
]

{ #category : #accessing }
VSCommand >> target [
	^ target
]

{ #category : #accessing }
VSCommand >> target: aConfigurationClass [
	target := aConfigurationClass
]
