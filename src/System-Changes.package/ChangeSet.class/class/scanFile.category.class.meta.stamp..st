scanning
scanFile: file category: cat class: class meta: meta stamp: stamp
	| itemPosition method items |
	items := OrderedCollection new.
	[itemPosition := file position.
	method := file nextChunk.
	file skipStyleChunk.
	method notEmpty] whileTrue:[
		items add: (ChangeRecord new file: file position: itemPosition type: #method
							class: class category: cat meta: meta stamp: stamp)].
	^items