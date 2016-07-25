fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^#(
    Uint32 format;
    SDL_Palette* palette;
    Uint8 BitsPerPixel;
    Uint8 BytesPerPixel;
    Uint8 pad1;
    Uint32 Rmask;
    Uint32 Gmask;
    Uint32 Bmask;
    Uint32 Amask;
    Uint8 Rloss;
    Uint8 Gloss;
    Uint8 Bloss;
    Uint8 Aloss;
    Uint8 Rshift;
    Uint8 Gshift;
    Uint8 Bshift;
    Uint8 Ashift;
    int refcount;
    SDL_PixelFormat* next;
 	)