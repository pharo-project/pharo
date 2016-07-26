public
get: url	
	| contents |
	contents := self httpClient get: url.
	^ String new: contents size streamContents: [ :stream |
		contents linesDo: [ :each | stream nextPutAll: each; cr ] ]