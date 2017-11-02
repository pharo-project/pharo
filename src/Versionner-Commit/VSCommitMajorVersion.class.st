"
I'm a major version.
"
Class {
	#name : #VSCommitMajorVersion,
	#superclass : #VSCommitVersion,
	#category : 'Versionner-Commit'
}

{ #category : #accessing }
VSCommitMajorVersion class >> label [
	^ 'Major'
]

{ #category : #accessing }
VSCommitMajorVersion >> nextVersionOf: aProject [
	| version |
	version := self semanticVersionFor: aProject.
	^ '{1}.0' format: { 
		(version at: 1) + 1 }
]
