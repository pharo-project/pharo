testing
hasDataDescriptor
	^ (self bitFlag bitAnd: 8)	~= 0 "GPBF:=HAS:=DATA:=DESCRIPTOR:=MASK"