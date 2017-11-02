"
I am an user interface used to link mcpackages and remotes

PackageRemotesManager new openWithSpec
"
Class {
	#name : #PackageRemotesManager,
	#superclass : #ComposablePresenter,
	#instVars : [
		'packages',
		'remotes',
		'silent'
	],
	#category : 'Komitter-Support'
}

{ #category : #specs }
PackageRemotesManager class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newRow: [ :row |
			row
				add: #packages;
				addSplitter;
				add: #remotes
		];
		yourself
]

{ #category : #private }
PackageRemotesManager >> change: selected forRemote: remote [
	| selectedPackage group |
	
	selectedPackage := packages selectedItem.
	selectedPackage ifNil: [ ^ self ].
	
	group := selectedPackage content repositoryGroup.
	
	selected
		ifTrue: [ group addRepository: remote ]
		ifFalse: [ group removeRepository: remote ]
]

{ #category : #private }
PackageRemotesManager >> doIfNotSilent: aBlock [

	silent ifTrue: [ ^ self ].
	aBlock value
]

{ #category : #api }
PackageRemotesManager >> initialExtent [

	^  700@460
]

{ #category : #initialization }
PackageRemotesManager >> initialize [

	super initialize.
	silent := false.
]

{ #category : #initialization }
PackageRemotesManager >> initializeDialogWindow: aWindow [

	aWindow
		toolbar: OkToolbar new
]

{ #category : #initialization }
PackageRemotesManager >> initializePresenter [

	packages whenSelectedItemChanged: [ :item |	self updateRemoteSelectionFrom: item ].
		
	packages whenRootsChanged: [ :nodes | 
		nodes ifNotEmpty: [ nodes first takeHighlight ] ].
	
	remotes selectedChangedBlock: [:remote :selected | 
		self doIfNotSilent: [ self change: selected forRemote: remote ]].
	
	self whenBuiltDo: [ | nodes |
		nodes := packages roots.
		nodes ifNotEmpty: [
			packages selectedItem: (
			nodes first 
				selected: true;
				takeHighlight;
				yourself ) ] ]
]

{ #category : #initialization }
PackageRemotesManager >> initializeWidgets [

	packages := self instantiate: SearchableTree.
	remotes := self instantiate: RemotesManager.

	packages displayBlock: [ :each | each packageName ].
	
	remotes selectedRemotes: self selectedPackageRepositories.
		
	self focusOrder
		add: packages;
		add: remotes
]

{ #category : #accessing }
PackageRemotesManager >> packages [

	^ packages
]

{ #category : #api }
PackageRemotesManager >> packages: mcPackages [

	packages roots: mcPackages
]

{ #category : #accessing }
PackageRemotesManager >> remotes [

	^ remotes
]

{ #category : #accessing }
PackageRemotesManager >> remotesTree [

	^ remotes remotes
]

{ #category : #private }
PackageRemotesManager >> selectedPackageRepositories [

	^ packages selectedItem
		ifNil: [ #() ]
		ifNotNil: [ :item | item content repositoryGroup repositories ].
]

{ #category : #private }
PackageRemotesManager >> silentWhile: aBlock [
	| oldSilent |
	
	oldSilent := silent.
	silent := true.
	aBlock ensure: [ silent := oldSilent ]
]

{ #category : #api }
PackageRemotesManager >> title [

	^ 'Remotes manager'
]

{ #category : #private }
PackageRemotesManager >> updateRemoteSelectionFrom: aPackage [
	| repositories |
	
	aPackage 
		ifNil: [ repositories := #() ]
		ifNotNil: [ repositories := aPackage repositoryGroup repositories ].

	self remotesTree roots do: [ :node |
		self silentWhile: [ 
			node selected: (repositories includes: node content remote) ] ]
]
