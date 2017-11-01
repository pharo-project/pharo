"
Commit work
"
Class {
	#name : #GoferCommit,
	#superclass : #GoferWorking,
	#instVars : [
		'message'
	],
	#category : 'Gofer-Core'
}

{ #category : #running }
GoferCommit >> execute [
	self workingCopies 
		do: [ :each | self execute: each ]
]

{ #category : #running }
GoferCommit >> execute: aWorkingCopy [
	| repositories version |
	repositories := self gofer repositories
		reject: [ :repository | (aWorkingCopy changesRelativeToRepository: repository) isEmpty ].
	repositories isEmpty
		ifTrue: [ ^ self ].
	version := [ aWorkingCopy newVersionIn: (MCRepositoryGroup withRepositories: repositories) ]
		on: MCVersionNameAndMessageRequest
		do: [ :notifcation |
			self message ifNil: [ message := notifcation outer last ].
			notifcation resume: (Array with: notifcation suggestedName with: self message) ].
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]
]

{ #category : #running }
GoferCommit >> initializeOn: aGofer [
	super initializeOn: aGofer disablePackageCache
]

{ #category : #accessing }
GoferCommit >> message [
	^ message
]

{ #category : #accessing }
GoferCommit >> message: aString [
	message := aString
]
