private
internalGetText
	| list |
	list := method stamp substrings.
	list ifEmpty: [ ^ 'No information available' ].
	^ self formattedModificationTextFor:(list first) atStamp:(list allButFirst joinUsing:' ')
