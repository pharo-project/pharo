public-enumerating
fileNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self fileNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].