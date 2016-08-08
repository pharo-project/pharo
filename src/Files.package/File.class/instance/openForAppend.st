open/close
openForAppend

	| stream |
	stream := self openForWrite: true.
	^ stream setToEnd; yourself