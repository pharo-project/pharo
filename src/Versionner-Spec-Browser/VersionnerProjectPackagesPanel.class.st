"
A VersionnerProjectPackagesPanel is the panel related to packages defined in a project (a configuration).
It shows packages and provides actions on them.

Instance Variables
	addPackageButton:		ButtonPresenter
	editPackageButton:		ButtonPresenter
	packageChangesButton:		ButtonPresenter
	packages: 			A collection of MTPackage to display
	packagesList:		IconListPresenter
	packagesLabel:		LabelPresenter
	project: 	MTProject
	savePackageButton:		ButtonPresenter

addPackageButton
	- Add a new package

editPackageButton
	- Edit an existing package

packageChangesButton
	- Get changes on a package

packages
	- the list of packages that belongs to a specfied project (configuration)

packagesLabel
	- The top label of this pane

project
 	- The project owning packages.
	
removePackageButton
	- remove a declared package.

savePackageButton
	- Commit changes and update package version

"
Class {
	#name : #VersionnerProjectPackagesPanel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'project',
		'packagesList',
		'packagesLabel',
		'addPackageButton',
		'removePackageButton',
		'editPackageButton',
		'savePackageButton'
	],
	#category : #'Versionner-Spec-Browser'
}

{ #category : #spec }
VersionnerProjectPackagesPanel class >> defaultSpec [
	<spec>

	"Define the layout of the Project Packages Panel"
	^ SpecLayout composed
		"newColumn: [ :c1 | c1
				newRow: [:r1 | r1
						add: #packagesLabel; 
						add: #addPackageButton;
						add: #removePackageButton;
						add: #editPackageButton;
						add: #savePackageButton]
					height: 25;
				add: #packages
		];"
	"Use the code below because Spec parser takes too much time to load Versionner with the previous declaration."
		add: #packagesLabel origin: 0@0 corner: 1/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #addPackageButton origin: 1/4@0 corner: 2/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #removePackageButton origin: 2/4@0 corner: 3/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		add: #editPackageButton origin: 3/4@0 corner: 4/4@0 offsetOrigin: 0@0 offsetCorner: 0@25;
		"add: #savePackageButton origin: 4/6@0 corner: 5/6@0 offsetOrigin: 0@0 offsetCorner: 0@25;"
		add: #packagesList origin: 0@0 corner: 1@1 offsetOrigin: 0@25 offsetCorner: 0@0;
	yourself.

]

{ #category : #actions }
VersionnerProjectPackagesPanel >> addPackage [
	"Add a package as internal dependency."
	| searchUI packageWC package |
	searchUI := SearchFacade 
		mcPackageSearchRejectAll:  [ :workingCopy || versionedPackages pkgName |
			versionedPackages := project packages collect: [ :each | each name ].
			pkgName := workingCopy package name.			
			(versionedPackages includes: pkgName) or: [pkgName beginsWith: 'ConfigurationOf'] ]
		withInitialFilter: project name.
	packageWC := searchUI chooseFromOwner: self window.

	packageWC
		ifNotNil: [ 
			package := MTPackage newNamed: packageWC package name.
			owner project addDependency: package.
			packagesList roots: project packages.
				"package ancestry ancestors isEmpty
				ifTrue: [ 
					(MBPlatform current
						confirm:
							'The package: ' , package package name printString , ' has not been committed yet. Would you like to commit it into the '
							, repository description printString, ' repository?')
						ifTrue: [ 
							package repositoryGroup repositories size <= 1
								ifTrue: [ package repositoryGroup addRepository: repository ].
							package currentVersionInfo ] ] " ]
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> addPackageButton [
	^ addPackageButton
]

{ #category : #actions }
VersionnerProjectPackagesPanel >> addPackageRequirement [
	"Add a requirement (a group, another package) for the selected package."
	
	packagesList selectedItem
		ifNil: [ UIManager default inform: 'No package selected' ]
		ifNotNil: [ :packageNode |
			|  potentialRequirements |
			potentialRequirements := project dependencies 
											reject: [ :item | | selectedPackage |
													selectedPackage := packageNode content.
													(item = selectedPackage) or: [ selectedPackage dependencies includes: item ] ]
											thenCollect: [ :dependency | dependency name ].
			^ UIManager default chooseFrom: potentialRequirements values: potentialRequirements ].
	^ nil
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> editPackageButton [
	^ editPackageButton
]

{ #category : #actions }
VersionnerProjectPackagesPanel >> editSelectedPackageRequirements [
	"Edit package requirements for the selected package"
	
	| selectedPackage editionWidget |

	selectedPackage := self packageBoundToSelection.
	selectedPackage ifNil: [ ^self ].
	editionWidget := (EditableList new: selectedPackage dependencies) 
		title: selectedPackage name , ' requirements';
		addItemBlock: [ self addPackageRequirement ];
		yourself.	
	editionWidget okAction: [ "propagate changes"
		selectedPackage dependencies: editionWidget list listItems.
		packagesList roots: project packages ].
	self window openModal: editionWidget openDialogWithSpec window
	
]

{ #category : #initialization }
VersionnerProjectPackagesPanel >> initializePresenter [
	"Initialize widgets behavior"

	packagesList whenSelectedNodesChanged: [:selection |
		removePackageButton enabled: (self isPackage: selection).
		editPackageButton enabled: selection notEmpty ].
		
	addPackageButton action: [ self addPackage ].
	removePackageButton action: [ self removeSelectedPackage ].
	editPackageButton action: [ self editSelectedPackageRequirements ].
]

{ #category : #initialization }
VersionnerProjectPackagesPanel >> initializeWidgets [
	packagesList := (self instantiate: TreePresenter)
		dragEnabled: true;
		whenBuiltDo: [ packagesList rootNodeHolder: [ :item | VSPackageNode new content: item ] ];
		yourself.  
	"packagesList icons: [:configuration | self iconForConfiguration: configuration ]. "
	"packagesList displayBlock: [:item |  |attributes text version|
							attributes := OrderedCollection new.
							text := item packageName.
							(item name size) > (text size) ifTrue: [
								version := (item name copyFrom: (text size + 2) to: (item name size)).
								(version size > 0) ifTrue: [ 
									text := text , ' (' , version , ')'. ].
								].
							item isDirty
								ifTrue: [ text := '*', text ].
							text.
						 ]."

	packagesLabel := self instantiate: LabelPresenter .
	packagesLabel label: 'Packages' .
	
	addPackageButton := self instantiate: ButtonPresenter .
	addPackageButton
		icon: (self iconNamed: #packageAddIcon);
		enabled: false;
		help: 'Add a package to this project'.
	
	removePackageButton  := self instantiate: ButtonPresenter .
	removePackageButton 
		icon: (self iconNamed: #packageDeleteIcon);
		enabled: false;
		help: 'Remove a package from this project'.
	
	editPackageButton  := self instantiate: ButtonPresenter .
	editPackageButton
		icon: (self iconNamed: #editIcon);
		enabled: false;
		help: 'Edit package requirements'.
	
	savePackageButton  := self instantiate: ButtonPresenter .
	savePackageButton
		icon: (self iconNamed: #smallSaveIcon);
		enabled: false;
		help: 'Save the selected package'.
]

{ #category : #testing }
VersionnerProjectPackagesPanel >> isPackage: selection [
	| node |
	
	(selection size = 1) ifFalse: [  ^false ].
	node := selection at: 1.
	^ node parentNode isNil
]

{ #category : #selection }
VersionnerProjectPackagesPanel >> packageBoundToSelection [
	| selection |
	
	selection := packagesList selectedItem.
	selection ifNil: [ ^ nil ].
	^ (selection parentNode)
		ifNil: [ selection content ]
		ifNotNil: [ selection parentNode content ]
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> packages [
	^ project ifNil: [ #() ] ifNotNil: [ project packages ] 
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> packagesLabel [
	^ packagesLabel
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> packagesList [
	^ packagesList
]

{ #category : #api }
VersionnerProjectPackagesPanel >> project: anMTProject [
	project := anMTProject.
	packagesList roots: self packages.
	project ifNotNil: 
		[ project announcer weak 
			when: MTProjectModelChanged
			send: #refreshPackages
			to: self ].
]

{ #category : #refreshing }
VersionnerProjectPackagesPanel >> refreshPackages [
	packagesList roots: project packages
]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> removePackageButton [
	^ removePackageButton
]

{ #category : #actions }
VersionnerProjectPackagesPanel >> removeSelectedPackage [
	"remove the selected package from the model and the list."
	| selectedPackage |

	selectedPackage := packagesList selectedItem.
	selectedPackage ifNil: [ ^self ].
	
	selectedPackage content remove.
	"index := packagesList selectedIndex."
	packagesList roots: project packages.
	"packagesList setSelectedIndex: index."
	

]

{ #category : #accessing }
VersionnerProjectPackagesPanel >> savePackageButton [
	^ savePackageButton
]

{ #category : #metacello }
VersionnerProjectPackagesPanel >> workingCopies [
	^ MCWorkingCopy allManagers asSortedCollection: [ :a :b | a name <= b name]
]
