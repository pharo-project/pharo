"
I'm a minor version.
"
Class {
	#name : #VSCommitMinorVersion,
	#superclass : #VSCommitVersion,
	#category : #Versionner-Commit
}

{ #category : #accessing }
VSCommitMinorVersion class >> label [
	^ 'Minor'
]

{ #category : #accessing }
VSCommitMinorVersion >> nextVersionOf: aProject [
	| version |
	version := self semanticVersionFor: aProject.
	^ '{1}.{2}' format: {
		(version at: 1). 
		(version at: 2) + 1 }
]
