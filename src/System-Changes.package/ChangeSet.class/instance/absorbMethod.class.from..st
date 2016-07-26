moving changes
absorbMethod: selector class: aClass from: aChangeSet
	"Absorb into the receiver all the changes for the method in the class in the other change set."

	| info |
	info := aChangeSet methodChanges at: aClass name ifAbsent: [Dictionary new].
	self atSelector: selector class: aClass put: (info at: selector).

