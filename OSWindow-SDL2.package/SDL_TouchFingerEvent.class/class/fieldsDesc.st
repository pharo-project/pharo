fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint64 touchId;
    Sint64 fingerId;
    float x;
    float y;
 	 float dx;
    float dy;
	 float pressure;
 	)	
	