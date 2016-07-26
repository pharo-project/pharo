undo-undo
unplugWhile: aBlock
	| wasPlugged |
	wasPlugged := self isPlugged.
	plugged := false.
	aBlock ensure: [ plugged := wasPlugged ]