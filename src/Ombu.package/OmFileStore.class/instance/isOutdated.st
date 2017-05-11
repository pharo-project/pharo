testing
isOutdated
	"Answer if #refresh is needed. To that end, I check if the file has different size than last time I wrote."

	^ self fileReference exists
		ifTrue: [ self fileReference size ~= lastStreamPosition ]
		ifFalse: [ lastStreamPosition isNotNil ]
