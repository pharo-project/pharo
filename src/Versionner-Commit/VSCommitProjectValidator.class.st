"
I'm a project validator. 
I check project configuration is ok. 
I check project development version is ok.
"
Class {
	#name : #VSCommitProjectValidator,
	#superclass : #Object,
	#instVars : [
		'project',
		'basename'
	],
	#category : #'Versionner-Commit'
}

{ #category : #'instance creation' }
VSCommitProjectValidator class >> basename: aString [
	^ self basicNew
		initializeBasename: aString;
		yourself
]

{ #category : #'instance creation' }
VSCommitProjectValidator class >> new [
	self error: 'Use #project:'
]

{ #category : #'instance creation' }
VSCommitProjectValidator class >> project: aProject [
	^ self basename: aProject name
]

{ #category : #accessing }
VSCommitProjectValidator >> basename [
	^ basename
]

{ #category : #initialization }
VSCommitProjectValidator >> initializeBasename: aString [
	basename := aString.
	self initialize
]

{ #category : #validation }
VSCommitProjectValidator >> validate [
	| toolBox |
	
	[	toolBox := MetacelloToolBox configurationNamed: self basename.
		toolBox validateVersion: #development ]
	on: MetacelloValidationFailure do: [ :e | 
		VSCommitProjectValidationError new
			projectName: self basename;
			issues: e issues;
			messageText: self basename, ': ', (e issues first explanation);
			signal ].
]
