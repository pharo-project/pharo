actions
convert: aSourcePath
	
	" iterate over segments and do conversion "
	
	| segment |
	
	segment := aSourcePath.
	
	[ segment notNil ] whileTrue: [
		segment convertWith: self.
		endPoint := segment endPoint.
		segment := segment next.
	].

	^ dest finish