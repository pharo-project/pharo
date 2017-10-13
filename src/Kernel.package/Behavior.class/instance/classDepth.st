accessing
classDepth

	self superclass ifNil: [^ 1].
	^ self superclass classDepth + 1