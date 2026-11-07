"
I am a command to show method history available from iceberg repository.
So I am only available for packages driven by Iceberg.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
	repository:		<IceRepository>
"
Class {
	#name : 'ClyIcebergShowMethodVersionCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'method'
	],
	#category : 'Calypso-SystemPlugins-Monticello-Browser',
	#package : 'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : 'testing' }
ClyIcebergShowMethodVersionCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isMethodSelected and: [
		  (IceRepository registeredRepositoryIncludingPackage:
			   aToolContext lastSelectedMethod package) isNotNil ]
]

{ #category : 'accessing' }
ClyIcebergShowMethodVersionCommand class >> methodBrowserTabActivation [
	<classAnnotation>

	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext
]

{ #category : 'execution' }
ClyIcebergShowMethodVersionCommand >> executeRefactoring [

	| browserClass packageName repositoriesWithPackage repository |
	packageName := method package name.
	repositoriesWithPackage := IceRepository registry select: [ :repo | repo includesPackageNamed: packageName ].
	repositoriesWithPackage ifEmpty: [
			self inform: 'No repositories include package ' , packageName.
			^ self ].
	repository := repositoriesWithPackage size > 1
		              ifTrue: [ self morphicUIManager chooseFrom: (repositoriesWithPackage collect: [ :each | each name ]) values: repositoriesWithPackage title: 'Repository' ]
		              ifFalse: [ repositoriesWithPackage anyOne ].
	repository ifNil: [ ^ self ].

	self flag: #pharoTodo.
	"This is a way to hide a dependency to Iceberg-IceTip package."
	browserClass := #IceTipVersionHistoryBrowser asClassInEnvironment: self class environment.

	(browserClass onRepository: repository method: method) open
]

{ #category : 'accessing' }
ClyIcebergShowMethodVersionCommand >> iconName [
	^#komitterSmalltalkhubRemote
]

{ #category : 'accessing' }
ClyIcebergShowMethodVersionCommand >> menuGroup [

	^ ClyQueryMenuGroup
]

{ #category : 'accessing' }
ClyIcebergShowMethodVersionCommand >> name [

	^ 'Commit history'
]

{ #category : 'accessing' }
ClyIcebergShowMethodVersionCommand >> order [

	^ 1001
]

{ #category : 'execution' }
ClyIcebergShowMethodVersionCommand >> prepareFullExecution [

	super prepareFullExecution.
	method := self context lastSelectedMethod
]
