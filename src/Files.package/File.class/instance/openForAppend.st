open/close
openForAppend

	| stream |
	stream := self openForWrite: true.
	^ stream setAtEnd; yourself