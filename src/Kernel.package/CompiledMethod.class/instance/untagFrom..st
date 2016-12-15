accessing-tags
untagFrom: aSymbol
	"Any method could be tagged with multiple symbols for user purpose. 
	This method should remove given tag from it. All other tags should not be changed. 
	But now we could implemented tags with protocols which allow only tag for method.
	And to remove tag from method we must change it protocol to Protocol unclassified"
	self protocol = aSymbol ifTrue: [ 
		self protocol: Protocol unclassified]