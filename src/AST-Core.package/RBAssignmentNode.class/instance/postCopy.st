copying
postCopy
	super postCopy.
	self variable: self variable copy.
	self value: self value copy