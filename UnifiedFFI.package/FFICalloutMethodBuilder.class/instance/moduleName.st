accessing
moduleName
	"Answer the module name, who can come from a string with the path to the module or a 
	 reference to a class who is child of FFILibrary"
	self module ifNil: [ ^ nil ].
	^ self module asFFILibrary moduleName