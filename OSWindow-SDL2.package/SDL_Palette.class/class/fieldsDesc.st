fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^#(
    int ncolors;
    SDL_Color* colors;
    Uint32 version;
    int refcount;
 	)