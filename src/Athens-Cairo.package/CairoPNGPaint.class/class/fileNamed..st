instance creation
fileNamed: aFileName
	| image|
	image := self new.
	image initializeOnFileNamed: aFileName.
	^ image