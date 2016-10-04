field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^#(
   	 	char *name;
    	Uint32 flags;
    	Uint32 num_texture_formats;
    	Uint32_16 texture_formats;
    	int max_texture_width;
    	int max_texture_height;
 	)