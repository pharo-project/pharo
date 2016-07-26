method changes
atSelector: selector class: class put: changeType

	(self changeRecorderFor: class) atSelector: selector put: changeType.
