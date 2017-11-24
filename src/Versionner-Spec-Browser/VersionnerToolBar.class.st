"
Versionner configuration list toolBar spec definition.

"
Class {
	#name : #VersionnerToolBar,
	#superclass : #ComposablePresenter,
	#instVars : [
		'newButton',
		'loadButton',
		'openButton'
	],
	#category : #'Versionner-Spec-Browser'
}

{ #category : #spec }
VersionnerToolBar class >> defaultSpec [
	<spec>

	"Define the layout of the Versionner configuration list toolBar"
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #newButton;
				add: #loadButton;
				add: #openButton ];
	yourself.
]

{ #category : #icons }
VersionnerToolBar class >> loadIcon [
	^ self iconNamed: #downIcon
]

{ #category : #icons }
VersionnerToolBar class >> newIcon [
	^ self iconNamed: #packageAddIcon
]

{ #category : #icons }
VersionnerToolBar class >> openIcon [
	^ self iconNamed: #openIcon
]

{ #category : #initialization }
VersionnerToolBar >> initializePresenter [
	"Initialize widgets behavior"

	newButton action: [ self newConfiguration ].
	loadButton action: [ (VSLoadProjectCommand target: self for: self) execute ].
	openButton action: [ owner configurationList selectedItem ifNotNil: [ :selection | 
				owner browseConfiguration: selection ]  ].
]

{ #category : #initialization }
VersionnerToolBar >> initializeWidgets [
	newButton := (self instantiate: ButtonPresenter)
		label: ' New project';
		icon: self class newIcon;
		help: 'Create a new project (configuration). Once created, double-click on the projet to edit it';
		yourself.
	
	loadButton := (self instantiate: ButtonPresenter)
		label: ' Load project';
		icon: self class loadIcon;
		help: 'Load an existing project (only its configuration) into the image';
		yourself.

	openButton := (self instantiate: ButtonPresenter)
		label: ' Open project';
		icon: self class openIcon;
		help: 'Open a new window on the selected project';
		yourself.

]

{ #category : #accessing }
VersionnerToolBar >> loadButton [
	^ loadButton
]

{ #category : #accessing }
VersionnerToolBar >> newButton [
	^ newButton
]

{ #category : #actions }
VersionnerToolBar >> newConfiguration [
	| name selection |
	name := UIManager default request: 'Name of the new project'.
	name isEmptyOrNil ifFalse: [ 
		Cursor execute showWhile:  [ | project |
			project := MTProject
				newNamed: name configurationBaseName
				withInitialVersion: '0.1'
				inRepository: nil.
			MCWorkingCopy forPackage: (MCPackage new name: project configurationClass name).
			selection :=  owner configurationList listItems 
					detect: [ :item | item name configurationBaseName = name configurationBaseName].
			owner configurationList setSelectedItem: selection.
			owner browseConfiguration: selection ]  ]
]

{ #category : #accessing }
VersionnerToolBar >> openButton [
	^ openButton
]
