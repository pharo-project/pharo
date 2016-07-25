public
sourcesFileOrNil
	| sources |
	sources := self openSources.
	^ (sources isNil or: [ sources atEnd ])
		ifTrue: [ 
			self informCannotLocateSources.
			nil ]
		ifFalse: [ sources ]