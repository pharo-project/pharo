modal dialogs
modalFolderSelector: aDir

	| window fileModel |
	window := (self newOnDirectory: aDir) morphicViewFolderSelector: aDir.
	fileModel := window model.
	window openInWorld: self currentWorld extent: 300@400.
	World openModal: window.
	^fileModel getSelectedFolder