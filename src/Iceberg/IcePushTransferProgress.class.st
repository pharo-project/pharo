Class {
	#name : #IcePushTransferProgress,
	#superclass : #LGitPushTransferProgressCallback,
	#instVars : [
		'job'
	],
	#category : #'Iceberg-Progress'
}

{ #category : #accessing }
IcePushTransferProgress >> block [
	^ [ :current :total :bytes :payload | 
		job ensureStarted.
		
		job min: 0.
		job max: total.
		job current: current.
		job title: ((self title, ' ({1}/{2})') format: { current. total}).
		
		job changed.
		
		0
	]
]

{ #category : #accessing }
IcePushTransferProgress >> initialize [
	super initialize.
	
	job := IceExternalJob new
		title: self title;
		yourself.
		
	self initializeBlock.
]

{ #category : #initialization }
IcePushTransferProgress >> initializeBlock [
	self signature: self class fnSpec block: self block.
]

{ #category : #accessing }
IcePushTransferProgress >> title [
	^ 'Pushing...'
]
