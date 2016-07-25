private
changesFromSliceCommitMessage

	| messageHeaderLines changesLines |
	messageHeaderLines := 3.
	changesLines := self sliceCommitMessage lines allButFirst: messageHeaderLines.
	^ changesLines joinUsing: Character cr