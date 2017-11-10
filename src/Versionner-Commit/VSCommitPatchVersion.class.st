"
I'm a patch version.

"
Class {
	#name : #VSCommitPatchVersion,
	#superclass : #VSCommitVersion,
	#category : #Versionner-Commit
}

{ #category : #accessing }
VSCommitPatchVersion class >> label [
	^ 'Patch'
]

{ #category : #accessing }
VSCommitPatchVersion >> nextVersionOf: aProject [
	| version |
	version := self semanticVersionFor: aProject.
	^ '{1}.{2}.{3}' format: {
		(version at: 1). 
		(version at: 2). 
		(version at: 3) + 1 }
]
