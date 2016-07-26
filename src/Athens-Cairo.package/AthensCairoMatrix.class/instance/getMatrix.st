accessing
getMatrix
	
	"Shall we answer an AthensAffineMatrix instead?
	
	^ AthensAffineMatrix new loadAffineTransform: self
	 "

	^ self copy