"
I'm a command to perform a merge  of current image sources with head commit.
"
Class {
	#name : #IceMergeWorkingCopyCommand,
	#superclass : #IceRepositoryCommand,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #execute }
IceMergeWorkingCopyCommand >> execute [
	| commit |
	commit := self repository headCommit.
	commit merge.
	(self repository workingCopy referenceCommit = commit)
		ifTrue: [ self beSuccess ]
]
