accessing
fileListIndex: anInteger
	"Select the file name having the given index, and display its contents."

	| item |
	self okToChange ifFalse: [^ self].
	listIndex := anInteger.
	listIndex = 0 
		ifFalse: [
			item := (list at: anInteger).
			item isDirectory
				ifTrue: [
					listIndex := 0.
					brevityState := #FileList.
					self addPath: item fullName.
					self directory: item ]
				ifFalse: [reference := item]].  "open the file selected"

	brevityState := #needToGetBrief.
	self changed: #fileListIndex.
	self changedContents.
	self updateButtonRow