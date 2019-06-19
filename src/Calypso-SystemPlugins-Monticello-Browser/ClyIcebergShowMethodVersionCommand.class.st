"
I am a command to show method history available from iceberg repository.
So I am only available for packages driven by Iceberg.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
	repository:		<IceRepository>
"
Class {
	#name : #ClyIcebergShowMethodVersionCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'method',
		'repository'
	],
	#category : #'Calypso-SystemPlugins-Monticello-Browser'
}

{ #category : #testing }
ClyIcebergShowMethodVersionCommand class >> canBeExecutedInContext: aToolContext [
	self flag: #todo. "Check for IceLog is temporal, to let time to update iceberg on Pharo 7.0a"
	^ aToolContext isMethodSelected 
		and: [ (Smalltalk globals includesKey: #IceLog)
		and: [ (IceRepository registeredRepositoryIncludingPackage: aToolContext lastSelectedMethod package) notNil ] ]
]

{ #category : #accessing }
ClyIcebergShowMethodVersionCommand class >> methodBrowserTabActivation [
	<classAnnotation>
	
	^ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext 
]

{ #category : #accessing }
ClyIcebergShowMethodVersionCommand class >> methodMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup order: 1001 for: ClyMethod asCalypsoItemContext 
]

{ #category : #accessing }
ClyIcebergShowMethodVersionCommand >> defaultMenuIconName [
	^#komitterSmalltalkhubRemote
]

{ #category : #accessing }
ClyIcebergShowMethodVersionCommand >> defaultMenuItemName [
	^'History'
]

{ #category : #execution }
ClyIcebergShowMethodVersionCommand >> execute [
	| browserClass |
	browserClass := #IceTipExtendedVersionBrowser 
		asClassInEnvironment: self class environment
		ifAbsent: [ #IceExtendedVersionBrowser asClass ].
		
	(browserClass repository: repository method: method) open
]

{ #category : #execution }
ClyIcebergShowMethodVersionCommand >> prepareFullExecutionInContext: aToolContext [
	| packageName |
	super prepareFullExecutionInContext: aToolContext.
	
	method := aToolContext lastSelectedMethod.
	packageName := method package name.
	repository := IceRepository registry detect: [ :each | each includesPackageNamed: packageName ]
]
