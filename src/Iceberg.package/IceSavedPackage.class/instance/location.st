accessing
location
	| location |
	 
	location := self repository location.
	(self directoryPathString substrings: '/')
		do: [ :each | location := location / each ].
	^ location