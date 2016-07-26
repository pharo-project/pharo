accessing
getMatrix
	"Answer an affine transformation matrix currently used by receiver.
	Note, that resulting matrix should be copied from receiver's data, i.e.
	no future modifications of receiver should affect the answered object.
	
	The default implementation is to answer a copy of receiver"
	
	^ self copy