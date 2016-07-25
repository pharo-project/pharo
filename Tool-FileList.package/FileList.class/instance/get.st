file menu action
get
	"Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel."

	Cursor read
		showWhile: [ 
			self okToChange
				ifFalse: [ ^ nil ].
			brevityState := brevityState == #briefHex
				ifTrue: [ #needToGetFullHex ]
				ifFalse: [ #needToGetFull ].
			self changedContents
			 ]