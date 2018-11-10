"
I'm a command that will generate several menu entries: two for each baseline existent in repository.
It will allow the install of baselines.

I reuse IceTipMetacelloInstalllBaselineDefaultCommand and IceTipMetacelloInstalllBaselineGroupCommand
"
Class {
	#name : #IceTipMetacelloRepositoryInstallBaselineCommand,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-Metacello'
}

{ #category : #accessing }
IceTipMetacelloRepositoryInstallBaselineCommand class >> baselinePrefix [
	^ 'BaselineOf'
]

{ #category : #activation }
IceTipMetacelloRepositoryInstallBaselineCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation 
			byItemOf: IceTipMetacelloMenuGroup
			order: 10
			for: IceTipRepositoryListContext
]

{ #category : #testing }
IceTipMetacelloRepositoryInstallBaselineCommand class >> canBeExecutedInContext: aToolContext [
	self flag: #todo. "Demeters law"
	^ aToolContext repositoryModel entity workingCopy packages
		anySatisfy: [ :each | each name beginsWith: self baselinePrefix ]
]

{ #category : #private }
IceTipMetacelloRepositoryInstallBaselineCommand >> allBaselines [
	^ (self repositoryModel entity workingCopy packages
		select: [ :each | each name beginsWith: 'BaselineOf' ])
		sorted: [ :a :b | a name < b name ]
]

{ #category : #execution }
IceTipMetacelloRepositoryInstallBaselineCommand >> execute [
	"Do nothing, I will distribute to elements created by #fillContextMenu:using:"
]

{ #category : #'context menu' }
IceTipMetacelloRepositoryInstallBaselineCommand >> fillContextMenu: aMenu using: aCommandActivator [
	
	self allBaselines do: [ :each |
		self fillDefaultMenu: aMenu using: aCommandActivator baseline: each.
		self fillGroupMenu: aMenu using: aCommandActivator baseline: each ]		

]

{ #category : #'context menu' }
IceTipMetacelloRepositoryInstallBaselineCommand >> fillDefaultMenu: aMenu using: aCommandActivator baseline: aPackage [
	| concreteCommand |
	
	concreteCommand := self newBaselineDefaultCommand: aPackage.
	self 
		fillMenu: aMenu
		title: ('Install baseline of {1} (default)' format: { self nameOfBaseline: aPackage })
		command: concreteCommand 
		activator: aCommandActivator
		baseline: aPackage
]

{ #category : #'context menu' }
IceTipMetacelloRepositoryInstallBaselineCommand >> fillGroupMenu: aMenu using: aCommandActivator baseline: aPackage [
	| concreteCommand |
	
	concreteCommand := self newBaselineGroupCommand: aPackage.
	self 
		fillMenu: aMenu
		title:  ('Install baseline of {1}...' format: { self nameOfBaseline: aPackage })
		command: concreteCommand 
		activator: aCommandActivator
		baseline: aPackage
]

{ #category : #'context menu' }
IceTipMetacelloRepositoryInstallBaselineCommand >> fillMenu: aMenu title: aString command: aCommand activator: aCommandActivator baseline: aPackage [
	| activator item |
	activator := aCommandActivator newForCommand: aCommand.
	item := aMenu 	
		add: aString
		target: activator
		selector: #executeCommand.
		
	aCommand setUpIconForMenuItem: item.
	aCommand description 
		ifNotNil: [ :d | item setBalloonText: d ]
]

{ #category : #private }
IceTipMetacelloRepositoryInstallBaselineCommand >> nameOfBaseline: baselinePackage [
	^ baselinePackage name allButFirst: self class baselinePrefix size
]

{ #category : #'private factory' }
IceTipMetacelloRepositoryInstallBaselineCommand >> newBaselineDefaultCommand: anIcePackage [ 
	^ IceTipMetacelloInstallBaselineDefaultCommand new 
		repositoryModel: self repositoryModel;
		package: anIcePackage;
		yourself
]

{ #category : #'private factory' }
IceTipMetacelloRepositoryInstallBaselineCommand >> newBaselineGroupCommand: anIcePackage [ 
	^ IceTipMetacelloInstallBaselineGroupCommand new 
		repositoryModel: self repositoryModel;
		package: anIcePackage;
		yourself
]
