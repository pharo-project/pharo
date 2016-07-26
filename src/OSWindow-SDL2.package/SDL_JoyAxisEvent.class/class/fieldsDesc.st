fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint32 which;
    Uint8 axis;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    Sint16 value;
    Uint16 padding4;
 	)