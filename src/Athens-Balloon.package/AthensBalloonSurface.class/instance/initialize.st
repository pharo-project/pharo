initialize-release
initialize
	pathTransform := AthensAffineTransform new.
	paintTransform := AthensAffineTransform new.
	
	imageTransform := AthensAffineTransform new " AthensGenericTransform new".
	fillTransform := AthensAffineTransform new.
	strokeTransform := AthensAffineTransform new.
	maskEnabled := false.