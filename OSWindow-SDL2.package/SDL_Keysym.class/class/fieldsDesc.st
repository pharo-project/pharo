fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^#(
    SDL_Scancode scancode;
    SDL_Keycode sym;
    Uint16 mod;
    Uint32 unused;
 	)