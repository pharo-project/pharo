transforming
transformBy: aMorphicTransform
	"Transform the receiver into a local coordinate system."
	position :=  aMorphicTransform globalPointToLocal: position.