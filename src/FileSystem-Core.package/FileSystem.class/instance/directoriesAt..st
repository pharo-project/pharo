public-enumerating
directoriesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoriesAt: aResolvable do: [ :path|
			out nextPut: path ]].