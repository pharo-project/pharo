accessing
text
	^ text ifNil: [ 
		text := self newCode.
		text aboutToStyle: true. ]