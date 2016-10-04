field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_Version version;
		int subsystem;
		SDL_SysWMinfo_InfoUnion info;
 	)