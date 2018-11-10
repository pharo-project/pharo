"
I perform a push of outgoing commits.

"
Class {
	#name : #IceTipPushBrowser,
	#superclass : #IceTipRemoteActionBrowser,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #'instance creation' }
IceTipPushBrowser class >> onRepositoryModel: aModel [
	^ self on: (IceTipPushModel 
		repositoryModel: aModel 
		on: aModel entity) 
		beCached	
]

{ #category : #accessing }
IceTipPushBrowser >> contextClass [
	^ IceTipPushContext
]

{ #category : #actions }
IceTipPushBrowser >> doPush [
	self model pushThen: [ 
		self window delete ]
]

{ #category : #initialization }
IceTipPushBrowser >> initializeToolbar [
	"cancel toolbar (for the moment)"
]

{ #category : #initialization }
IceTipPushBrowser >> initializeWidgets [
	super initializeWidgets.
	remotePanel label: 'Push to remote:'
]

{ #category : #'private factory' }
IceTipPushBrowser >> newActionButton [
	^ self newButton 
		label: 'Push';
		icon: (self iconNamed: #up);
		help: ('Push new commits to {1}' format: { self model remoteName });
		action: [ self doPush ];
		yourself
]

{ #category : #accessing }
IceTipPushBrowser >> title [
	^ 'Push {1}/{2} to {3}' format: { 
		self model repositoryName.
		self model branchName. 
		self model remoteName }
]
