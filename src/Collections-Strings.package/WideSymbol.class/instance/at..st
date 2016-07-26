accessing
at: index 
	"Answer the Character stored in the field of the receiver indexed by the argument."
	^ Character value: (self wordAt: index).
