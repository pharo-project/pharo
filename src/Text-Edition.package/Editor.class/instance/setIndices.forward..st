private
setIndices: shiftPressed forward: forward
	"Little helper method that sets the moving and fixed indices according to some flags."
	| indices |
	indices := Dictionary new.
	self flag: 'to be reviewed'.
	(shiftPressed)
		ifTrue: [
			indices at: #moving put: self pointIndex.
			indices at: #fixed put: self markIndex
		] ifFalse: [
			forward
				ifTrue:[
					indices at: #moving put: self stopIndex.
					indices at: #fixed put: self startIndex.
				] ifFalse: [
					indices at: #moving put: self startIndex.
					indices at: #fixed put: self stopIndex.
				]
		].
	^indices