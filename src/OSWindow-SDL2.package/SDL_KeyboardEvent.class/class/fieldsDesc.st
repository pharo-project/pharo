as yet unclassified
fieldsDesc
	"
	self initializeAccessors
	"
	^ #(
	Uint32 type;
	Uint32 timestamp;
	Uint32 windowID;
	Uint8 state;
	Uint8 repeat;
    	Uint8 padding2;
    	Uint8 padding3;
    	SDL_Keysym keysym;
 	)