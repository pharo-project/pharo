"
I open commit tool to create a MAJOR version of target configuration.
"
Class {
	#name : #VSCommitMajorCommand,
	#superclass : #VSCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #execute }
VSCommitMajorCommand >> execute [
	(VSCommitUnit major: target configurationClass) open
]

{ #category : #ordering }
VSCommitMajorCommand >> order [
	"I want them to appear at the beginning, and in certain order"
	^ '03'
]

{ #category : #accessing }
VSCommitMajorCommand >> title [
	^ 'commit major version'
]
