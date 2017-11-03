"
I'm a project action who makes a new configuration for a project and commits it to repository.
"
Class {
	#name : #VSCommitProjectAction,
	#superclass : #VSCommitAction,
	#instVars : [
		'project',
		'commitVersion',
		'nextVersion'
	],
	#category : #Versionner-Commit
}

{ #category : #'instance creation' }
VSCommitProjectAction class >> project: aMTProject [ 
	^ self basicNew
		initializeProject: aMTProject;
		yourself
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> browseOn: aBrowser [
	aBrowser browseProject: self
]

{ #category : #accessing }
VSCommitProjectAction >> configurationClass [
	^ self project configurationClass
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> currentString [
	^ [ self project stableVersion ifNotNil: [  :version | version versionString  ] ]
	on: Error 
	do: [ :e | '' ]
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> displayString [
	^ self project name
]

{ #category : #executing }
VSCommitProjectAction >> executeOn: aUnit [
	| version |
	version := self nextVersionOn: aUnit.
	self project devWorkflow 
		releaseFixedDevelopment: version
		description: aUnit comment.
	self 
		commitPackage: self packageName 
		comment: aUnit comment
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> icon [
	^ Smalltalk ui icons iconNamed: #config
]

{ #category : #initialization }
VSCommitProjectAction >> initializeProject: aMTProject [ 
	project := aMTProject.
	self initialize
]

{ #category : #testing }
VSCommitProjectAction >> isProject [
	^ true
]

{ #category : #accessing }
VSCommitProjectAction >> nextVersion: anObject [
	^ nextVersion := anObject
]

{ #category : #accessing }
VSCommitProjectAction >> nextVersionOn: aUnit [ 
	^ nextVersion ifNil: [ aUnit commitVersion nextVersionOf: self project ]
]

{ #category : #accessing }
VSCommitProjectAction >> packageName [
	^ self project configurationClass package name
]

{ #category : #accessing }
VSCommitProjectAction >> project [
	^ project
]

{ #category : #accessing }
VSCommitProjectAction >> removeOn: aUnit [
	aUnit removeProjectAction: self
]

{ #category : #accessing }
VSCommitProjectAction >> repository [
	^ self project repository
]

{ #category : #validation }
VSCommitProjectAction >> validate [
	(VSCommitProjectValidator project: self project) validate
]

{ #category : #private }
VSCommitProjectAction >> workingCopy [ 
	^ self project workingCopy
]
