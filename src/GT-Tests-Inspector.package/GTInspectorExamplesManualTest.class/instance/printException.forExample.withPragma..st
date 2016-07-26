private
printException: anException forExample: anExample withPragma: aPragma
	(FileSystem workingDirectory / 'gtExamplesAllFailures.txt') writeStream 
		setToEnd; cr;
		nextPutAll: anExample method gtDisplayString; cr;
		flush;
		close.
	(FileSystem workingDirectory / (self filenameFor: anExample)) writeStream 
		truncate;
		nextPutAll: anExample gtDisplayString; cr;
		nextPutAll: aPragma gtDisplayString; cr;
		nextPutAll: anException asString; cr;
		nextPutAll: '--------------------------------------------------------------------------------'; cr;
		nextPutAll: (String streamContents: [ :stream | 
							anException signalerContext ifNotNil: [ :context | 
								context debugStackOn: stream ] ] );
		flush;
		close