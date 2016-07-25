accessing
author
	| author |
	self assurePreambleExists.
	author := self preambleString lineNumber: 3.
	author := author copyFrom: 8 to: author size. "Strip the 'Author:' prefix. Ugly ugly."	
	^ author trimBoth