"
I perform a pull of incoming commits.

"
Class {
	#name : #IceTipPullBrowser,
	#superclass : #IceTipRemoteActionBrowser,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #specs }
IceTipPullBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :mainColumn |
			mainColumn
				newRow: #toolbar height: IceTipToolbar defaultHeight;
				newRow: #remotePanel height: self inputTextHeight;
				newRow: [ :row |
					row newColumn: [ :column |
						column 
							add: #commitsPanel withSpec: #panelSpec;
							add: #actionPanel height: self iceActionPanelHeight ] ] ]
]

{ #category : #'instance creation' }
IceTipPullBrowser class >> onRepositoryModel: aModel [
	^ self on: (IceTipPullModel 
		repositoryModel: aModel 
		on: aModel entity) 
		beCached
]

{ #category : #accessing }
IceTipPullBrowser >> contextClass [
	^ IceTipPullContext
]

{ #category : #actions }
IceTipPullBrowser >> doPull [
	self model pullThen: [ 
		self window delete ]
]

{ #category : #initialization }
IceTipPullBrowser >> initializeWidgets [
	super initializeWidgets.
	remotePanel label: 'Pull from remote:'.
]

{ #category : #'private factory' }
IceTipPullBrowser >> newActionButton [
	^ self newButton 
		label: 'Pull';
		icon: (self iconNamed: #down);
		help: ('Pull new commits from {1}' format: { self model remoteName });
		action: [ self doPull ];
		yourself
]

{ #category : #accessing }
IceTipPullBrowser >> title [
	^ 'Pull {1}/{2} from {3}' format: { 
		self model repositoryName.
		self model branchName. 
		self model remoteName }
]
