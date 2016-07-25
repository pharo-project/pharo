modal dialogs
morphicViewFolderSelector: aDir
	"Answer a tool that allows the user to select a folder"
	|  window buttonRow fileListTopOffset |
	self directory: aDir.
	self optionalButtonSpecs: self servicesForFolderSelector.
	window := (SystemWindow labelled: aDir fullName) model: self.
	self modalView: window.
	buttonRow := self folderSelectButtonRow.
	self universalButtonServices do: 
			[:service |
				buttonRow addMorphBack: (service buttonToTriggerIn: self)].
	fileListTopOffset := (TextStyle defaultFont pointSize * 2) + 11.
	
	self addFullPanesTo: window from: {
		{self morphicDirectoryTreePane. (0@0 corner: 1@1) asLayoutFrame topOffset: (fileListTopOffset)}.
		{buttonRow  . (0@0 corner: 1@0) asLayoutFrame bottomOffset: (fileListTopOffset-3)}.
	}.
	
	self postOpen.
	^ window 