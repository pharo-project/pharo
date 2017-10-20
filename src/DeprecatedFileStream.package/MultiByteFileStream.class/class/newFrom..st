instance creation
newFrom: aFileStream

	| rw n |
	n := aFileStream name.
	rw := aFileStream isReadOnly not.
	aFileStream close.
	^self new open: n forWrite: rw.
