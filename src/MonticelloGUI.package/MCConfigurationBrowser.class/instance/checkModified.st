dependencies
checkModified
	| modified |
	modified := self dependencies select: [:dep |
		dep isFulfilled and: [dep package workingCopy modified]].
	
	^modified isEmpty or: [
		self selectDependency: modified anyOne.
		self confirm: (String streamContents: [:strm |
			strm nextPutAll: 'These packages are modified:'; cr.
			modified do: [:dep | strm nextPutAll: dep package name; cr].
			strm nextPutAll: 'Do you still want to store?'])]
	