private
announceLoad: aString do: aBlock
	| returnValue |
	returnValue := nil.
	[ self announceLoadStart: aString ] ensure: [ 
		[ returnValue := aBlock value ] ensure: [ 
			self announceLoadStop: aString ] ].
	^ returnValue