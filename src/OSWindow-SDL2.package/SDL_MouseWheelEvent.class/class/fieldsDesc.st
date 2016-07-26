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
    Sint32 x;
    Sint32 y;
 	)