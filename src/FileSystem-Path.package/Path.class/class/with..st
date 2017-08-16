private
with: aString
	"Answer an instance of the receiver representing the supplied string.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := self delimeter split: aString.
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst