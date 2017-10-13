menu
compareTo:anOtherVersion withLabel:aLabel
	| diff versions selected|
	selected := browser selectedMessage.
	versions := Array with:anOtherVersion with: (selected).
	diff := DiffPresenter new.
	diff	title: ('Comparison from {1} to {2}' format:{selected stamp . aLabel}).
	diff text:(DiffMorphChangeRecordConverter 
		methodReference: anOtherVersion 
		referencesList: versions) getText.
      diff openWithSpec.
