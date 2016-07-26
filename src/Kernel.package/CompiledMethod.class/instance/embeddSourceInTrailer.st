source code management
embeddSourceInTrailer
	"When receiver is deinstalled from its class, its not managed anymore by development tools
	and it's hard to predict, how long a method could stay in the image, because if it contains blocks,
	they could still reference it. 
	Therefore we trying to preserve as much as we can , actually by embedding the method's source code into its trailer
	"
	self trailer hasSourcePointer ifTrue: [
		^self becomeForward: (self copyWithSource: self sourceCode)]
