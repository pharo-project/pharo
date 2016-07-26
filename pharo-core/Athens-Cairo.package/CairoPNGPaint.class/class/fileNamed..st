as yet unclassified
fileNamed: aFileName
	| image|
	image := self new.
	image initializeOnFileNamed: aFileName.
	^ image