field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint32 which;
 	)