public-enumerating
directoryNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoryNamesAt: aResolvable do: [ :name|
			out nextPut: name ]].