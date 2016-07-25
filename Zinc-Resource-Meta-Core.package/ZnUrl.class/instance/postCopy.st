copying
postCopy
	super postCopy.
	segments := segments copy.
	query := query copy