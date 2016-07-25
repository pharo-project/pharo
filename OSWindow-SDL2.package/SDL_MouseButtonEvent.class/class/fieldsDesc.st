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
    Uint8 button;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
    Sint32 x;
    Sint32 y;
 	)