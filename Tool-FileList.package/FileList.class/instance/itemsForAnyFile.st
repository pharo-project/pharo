file list menu
itemsForAnyFile
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services := OrderedCollection new: 4.
	services add: self serviceCopyName. 
	services add: self serviceRenameFile. 
	services add: self serviceDeleteFile.
	services add: self serviceViewContents.
	services add: self serviceViewContentsInWorkspace.
	^ services