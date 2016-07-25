accessing
selectChildren: aBlock
	"With this block you can control how the guide spreads over directories.
	Example: 
		self selectChildren: [ :parentEntry | parentEntry isSymlink not ].
		This will prevent the outer visitor to see any children of symlinked directories.
		
	Since the guides essentially rearrange the files visited controlling which children
	you see is the main concern of the guide. All the other visiting aspects can be 
	controlled in the visitor."
	selectChildren := aBlock