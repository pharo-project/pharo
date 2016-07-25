public-enumerating
childNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self childNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].