"
I present a list of available metalink for a given ast node to be installed or uninstalled.
The presenter can be in install mode or uninstall mode, see class methods.
The list of links for a node is either all the instances of metalinks from the system when in install mode or all node currently installed on that node.
"
Class {
	#name : 'ClyMetaLinkInstallationPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'sourceNode',
		'toolBar',
		'isForMetaLinkInstallation',
		'metalinkListPresenter'
	],
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Metalinks',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Metalinks'
}

{ #category : 'layout' }
ClyMetaLinkInstallationPresenter class >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		  add: #toolBar
		  expand: false
		  fill: false
		  padding: 0;
		  add: #metalinkListPresenter;
		  yourself
]

{ #category : 'instance creation' }
ClyMetaLinkInstallationPresenter class >> onNode: aSourceNode forInstallation: forInstallation [
	^ self basicNew
		  sourceNode: aSourceNode;
		  isForMetaLinkInstallation: forInstallation;
		  initialize
]

{ #category : 'opening' }
ClyMetaLinkInstallationPresenter class >> openInstallerOnNode: aSourceNode [

	(self onNode: aSourceNode forInstallation: true) open
]

{ #category : 'opening' }
ClyMetaLinkInstallationPresenter class >> openUninstallerOnNode: aSourceNode [
	(self onNode: aSourceNode forInstallation: false) open
]

{ #category : 'accessing' }
ClyMetaLinkInstallationPresenter >> allRelevantMetaLinks [

	| baseLinks |
	baseLinks := self isForMetaLinkInstallation
		             ifTrue: [ "This is temporary, I should have put a comment.
The idea would be to add an explicit option at the metalink level, so that only the ones that are ''installable through the menu'' would appear.
But we need to think about how to do that exactly."
			             MetaLink allInstances select: [ :link |
				             link checkForCompatibilityWith: sourceNode ] ]
		             ifFalse: [ sourceNode links ].
	^ baseLinks reject: [ :link |
		  link metaObject == Break or: [
			  {
				  Watch.
				  ExecutionCounter } includes: link metaObject class ] ]
]

{ #category : 'initialization' }
ClyMetaLinkInstallationPresenter >> close [
	self withWindowDo: #close
]

{ #category : 'initialization' }
ClyMetaLinkInstallationPresenter >> initializePresenter [
	metalinkListPresenter := self newList.
	metalinkListPresenter items: self allRelevantMetaLinks.
	toolBar := self newToolbar.
	self toolbarButtons do: [ :button | toolBar addItem: button ]
]

{ #category : 'initialization' }
ClyMetaLinkInstallationPresenter >> initializeWindow: aWindowPresenter [
	
	super initializeWindow: aWindowPresenter.
	aWindowPresenter title: sourceNode printString
]

{ #category : 'installation' }
ClyMetaLinkInstallationPresenter >> installSelectedMetalink [
	metalinkListPresenter selection selectedItem ifNotNil: [ :metalink |
		sourceNode link: metalink ].
	self close
]

{ #category : 'testing' }
ClyMetaLinkInstallationPresenter >> isForMetaLinkInstallation [
	^isForMetaLinkInstallation ifNil:[isForMetaLinkInstallation := true]
]

{ #category : 'accessing' }
ClyMetaLinkInstallationPresenter >> isForMetaLinkInstallation: anObject [
	isForMetaLinkInstallation := anObject
]

{ #category : 'accessing' }
ClyMetaLinkInstallationPresenter >> metalinkListPresenter [
	^ metalinkListPresenter
]

{ #category : 'accessing' }
ClyMetaLinkInstallationPresenter >> sourceNode: anObject [
	sourceNode := anObject
]

{ #category : 'accessing' }
ClyMetaLinkInstallationPresenter >> toolbarButtons [
	| buttons |
	buttons := OrderedCollection new.
	buttons add: (self isForMetaLinkInstallation
			 ifTrue: [
				 self newToolbarButton
					 label: 'Install';
					 icon: (self iconNamed: #smallOk);
					 help: 'Install the selected metalink';
					 action: [ self installSelectedMetalink ];
					 yourself ]
			 ifFalse: [
				 self newToolbarButton
					 label: 'Uninstall';
					 icon: (self iconNamed: #smallOk);
					 help: 'Uninstall the selected metalink';
					 action: [ self uninstallSelectedMetalink ];
					 yourself ]).
	buttons add: (self newToolbarButton
			 label: 'Cancel';
			 icon: (self iconNamed: #smallCancel);
			 help: 'Cancel';
			 action: [ self close ];
			 yourself).

	^ buttons
]

{ #category : 'installation' }
ClyMetaLinkInstallationPresenter >> uninstallSelectedMetalink [
	metalinkListPresenter selection selectedItem ifNotNil: [ :metalink |
		sourceNode removeLink: metalink ].
	self close
]
