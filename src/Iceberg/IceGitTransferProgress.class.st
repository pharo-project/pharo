Class {
	#name : #IceGitTransferProgress,
	#superclass : #LGitTransferProgressCallback,
	#instVars : [
		'job'
	],
	#category : #'Iceberg-Progress'
}

{ #category : #accessing }
IceGitTransferProgress >> block [
	^ [ :stats :payload | 
		| gitStats |
		gitStats := LGitTransferProgress fromHandle: stats.
		job ensureStarted.
		
		job min: 0.
		job max: gitStats prim_total_objects.
		job current: gitStats prim_received_objects.
		job title: ('Fetching Git objects... ({1}/{2})' format: { gitStats prim_received_objects. gitStats prim_total_objects}).
		
		job changed.
		
		0
	]
]

{ #category : #accessing }
IceGitTransferProgress >> initialize [
	super initialize.
	
	job := IceExternalJob new
		title: 'Fetching Git objects...';
		yourself.
		
	self initializeBlock.
]

{ #category : #'initialize - release' }
IceGitTransferProgress >> initializeBlock [
	self signature: self class fnSpec block: self block.
]
