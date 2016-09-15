operations
timeString
	| aDateTime |
	aDateTime := self log timeAt: entry ifAbsent: [ ^ '' ].
	
	^ aDateTime epiceaBrowsersAsString