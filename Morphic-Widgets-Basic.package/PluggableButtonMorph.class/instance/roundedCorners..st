accessing
roundedCorners: anArray
	"Adjust the layout inset if necessary."

	super roundedCorners: anArray.
	self layoutInset: (self theme buttonLabelInsetFor: self)