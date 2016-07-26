fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID; 
    Uint32 which;
    Uint32 state;
    Sint32 x;
    Sint32 y;
    Sint32 xrel;
    Sint32 yrel;
 	)