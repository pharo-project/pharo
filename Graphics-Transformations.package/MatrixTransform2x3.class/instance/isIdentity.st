testing
isIdentity
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	<primitive: 'primitiveIsIdentity' module: 'Matrix2x3Plugin'>
	^self isPureTranslation and:[self a13 = 0.0 and:[self a23 = 0.0]]