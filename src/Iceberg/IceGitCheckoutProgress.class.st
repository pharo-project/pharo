Class {
	#name : #IceGitCheckoutProgress,
	#superclass : #LGitCheckoutProgressCallback,
	#instVars : [
		'job'
	],
	#category : #'Iceberg-Progress'
}

{ #category : #accessing }
IceGitCheckoutProgress >> block [
	^ [ :path :completed :total :payload | 
		job ensureStarted.
		
		job min: 0.
		job max: total.
		job current: completed.
		job title: ('Checking out... ({1}/{2})' format: { completed. total}).
		
		job changed.
	]
]

{ #category : #initialization }
IceGitCheckoutProgress >> initialize [
	super initialize.
	
	job := IceExternalJob new
		title: 'Checking out...';
		yourself.
		
	self initializeBlock.
]

{ #category : #initialization }
IceGitCheckoutProgress >> initializeBlock [
	self signature: self class fnSpec block: self block.
]
