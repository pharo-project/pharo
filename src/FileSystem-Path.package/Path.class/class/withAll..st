private
withAll: aCollection
	"Answer an instance of the receiver representing the supplied collection of strings.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := OrderedCollection new: (aCollection size max: 10).
	aCollection do: [ :each |
		parsedCollection addAll: (self delimeter split: each) ].
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst