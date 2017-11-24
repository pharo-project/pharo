"
I open commit tool to create a PATCH version of target configuration.
"
Class {
	#name : #VSCommitPatchCommand,
	#superclass : #VSCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSCommitPatchCommand >> execute [
	(VSCommitUnit patch: target configurationClass) open
]

{ #category : #ordering }
VSCommitPatchCommand >> order [
	"I want them to appear at the beginning, and in certain order"
	^ '01'
]

{ #category : #accessing }
VSCommitPatchCommand >> title [
	^ 'commit patch version'
]
