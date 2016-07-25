fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint32 which;
    Uint8 button;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
 	)