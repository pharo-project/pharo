"
I'm a log version. 
I keep a commit and a method definition from each method version tracked on IceLog>>#historyOfMethod:.
"
Class {
	#name : #IceLogVersion,
	#superclass : #Object,
	#instVars : [
		'commit',
		'definition'
	],
	#category : #'Iceberg-Core'
}

{ #category : #'instance creation' }
IceLogVersion class >> commit: aCommit definition: aRingDefinition [ 
	^ self new 
		commit: aCommit; 
		definition: aRingDefinition;
		yourself
]

{ #category : #accessing }
IceLogVersion >> ancestors [
	^ self commit ancestors
]

{ #category : #accessing }
IceLogVersion >> author [
	^ self commit author
]

{ #category : #accessing }
IceLogVersion >> comment [
	^ self commit comment
]

{ #category : #accessing }
IceLogVersion >> commit [
	^ commit
]

{ #category : #accessing }
IceLogVersion >> commit: aCommit [
	commit := aCommit
]

{ #category : #accessing }
IceLogVersion >> datetime [
	^ self commit datetime
]

{ #category : #accessing }
IceLogVersion >> definition [
	^ definition
]

{ #category : #accessing }
IceLogVersion >> definition: aRingDefinition [
	definition := aRingDefinition
]

{ #category : #accessing }
IceLogVersion >> description [
	^ '{1} at {2}' format: { self author. self datetime asLocalStringYMDHM }
]

{ #category : #accessing }
IceLogVersion >> id [
	^ self commit id
]

{ #category : #accessing }
IceLogVersion >> originMethod [
	^ (Smalltalk at: self definition methodClass name) >> self definition selector
]

{ #category : #accessing }
IceLogVersion >> shortId [
	^ self commit shortId
]

{ #category : #accessing }
IceLogVersion >> source [
	^ String streamContents: [ :stream | 
		stream 
			<< '"protocol: ' << self definition category << '"'; cr;
			<< self definition sourceCode ]
]

{ #category : #accessing }
IceLogVersion >> tagNames [
	^ self commit tagNames
]

{ #category : #accessing }
IceLogVersion >> tags [
	^ self commit tags
]

{ #category : #accessing }
IceLogVersion >> timeStamp [
	^ self commit timeStamp
]
