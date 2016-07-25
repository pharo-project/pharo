method changes
atSelector: selector put: changeType

	(self findOrMakeMethodChangeAt: selector priorMethod: nil)
		noteChangeType: changeType