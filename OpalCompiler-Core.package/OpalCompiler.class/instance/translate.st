public access
translate
	[ self compile ] on: ReparseAfterSourceEditing do: [ :ex |
		self source: ex newSource readStream.
		self compile
	 ] .
	^ ast