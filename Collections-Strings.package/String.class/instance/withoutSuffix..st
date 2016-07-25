converting
withoutSuffix: suffix
	"Remove the given suffix, if present."

	^(self endsWith: suffix) ifTrue: [ self copyFrom: 1 to: self size - suffix size ] ifFalse: [ self ]