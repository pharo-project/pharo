transforming
translatedBy: delta
	"add delta to cursorPoint, and return the new event"
	^self shallowCopy translateBy: delta