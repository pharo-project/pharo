copying
postCopy
	"After copying we must duplicate any associations and pragmas so they don't end up being shared."
	1 to: self basicSize do:
		[:i| self basicAt: i put: (self basicAt: i) shallowCopy]