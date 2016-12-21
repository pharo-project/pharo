accessing-tags
tagWith: aSymbol
	"Any method could be tagged with multiple symbols for user purpose. 
	This method should apply new tag. All existing tags should not be changed. 
	But now we could only implemented tags with protocols. 	
	So tagging method with tag removes all existing tags and add new one. 
	It should not be problem with single tag scenario which are now defined by single protocol"
	
	self protocol: aSymbol