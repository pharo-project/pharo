accessing
installFontsIn: provider
	self canBeInstalled ifFalse: [ ^ self ].
	provider
		addFromFileContents: self fontContents 
		baseName: self originalFileName 