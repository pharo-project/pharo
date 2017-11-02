"
I open commit tool to create a MINOR version of target configuration.
"
Class {
	#name : #VSCommitMinorCommand,
	#superclass : #VSCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #execute }
VSCommitMinorCommand >> execute [
	(VSCommitUnit minor: target configurationClass) open
]

{ #category : #ordering }
VSCommitMinorCommand >> order [
	"I want them to appear at the beginning, and in certain order"
	^ '02'
]

{ #category : #accessing }
VSCommitMinorCommand >> title [
	^ 'commit minor version'
]
