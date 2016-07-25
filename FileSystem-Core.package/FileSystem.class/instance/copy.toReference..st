private
copy: aPath toReference: destRef
	| inputStream path |
	
	path := self resolve: aPath.
	
	[
	inputStream := self readStreamOn: path.
	inputStream ifNil: [ store signalFileDoesNotExist: path ].
	destRef fileSystem copyFrom: inputStream to: destRef path 
	
		] ensure: [ inputStream ifNotNil: [ inputStream close ]]