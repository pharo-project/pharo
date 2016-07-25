private
setCollection: anArray

	super setCollection: anArray.
	equalitySet := OCLiteralSet new: anArray size.
