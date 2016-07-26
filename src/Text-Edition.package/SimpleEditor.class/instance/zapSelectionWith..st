mvc compatibility
zapSelectionWith: aString

	| start stop |
	start := self startIndex.
	stop := self stopIndex.
	(start = stop and: [aString size = 0]) ifFalse: [
		string := string copyReplaceFrom: start to: stop-1 with: aString.
		self
			markIndex: start; 
			pointIndex: start + aString size.

		self userHasEdited  " -- note text now dirty" ]