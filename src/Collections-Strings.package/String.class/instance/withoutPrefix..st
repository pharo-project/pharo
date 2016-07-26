converting
withoutPrefix: prefix
	"Remove the given prefix, if present."

	^(self beginsWith: prefix) ifTrue: [ self copyFrom: 1 + prefix size to: self size ] ifFalse: [ self ]