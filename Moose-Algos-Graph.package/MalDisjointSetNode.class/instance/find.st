accessing
find

	^ (self = parent) ifTrue: [ self ]
					ifFalse: [ parent find ]