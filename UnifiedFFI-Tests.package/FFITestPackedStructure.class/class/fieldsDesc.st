field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^ #(
		byte byte;
		short short;
		long long;
		float float;
		double double;
		int64 int64;
		)