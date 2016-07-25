configuration
collection: aCollection caseSensitive: aBoolean
	"Configure this completion dialog with aCollection and match the elements case sensitive if aBoolean is true. Display the complete list if the filter is empty."

	self searchBlock: [ :value |
		value isEmpty
			ifTrue: [ aCollection ]
			ifFalse: [
				aCollection select: [ :each |
					self class
						substring: value
						matches: (self labelFor: each)
						caseSensitive: aBoolean ] ] ]