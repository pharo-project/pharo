copying
postCopy
	super postCopy.
	1 to: self basicSize do: [:i | self basicAt: i put: (self basicAt: i) copy]