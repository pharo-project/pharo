*Collections-arithmetic
+* aCollection
	"Premultiply aCollection by self.  aCollection should be an Array or Matrix.
	 The name of this method is APL's +.x squished into Smalltalk syntax."

	^aCollection preMultiplyByArray: self
