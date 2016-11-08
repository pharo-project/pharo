menu protocol
iconName: aSymbol
	"instead of forcing clients to refer to an icon builder such Smalltalk ui icons 
	this message encapsulates it inside the builder itself. When removing uses of Smalltalk ui icons it avoid to force to subclass class with menu to inherit from Model."
	
	self icon: (Smalltalk ui icons iconNamed: aSymbol)