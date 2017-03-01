private
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	list := self listForPattern: pattern.
	listIndex := self getListIndex.
	volumeListIndex := volumeList size.
	contents := ''.
	self changed: #volumeListIndex.
	self changed: #list.
	self grid deselectAll.
	self grid updateList.
	self updateButtonRow