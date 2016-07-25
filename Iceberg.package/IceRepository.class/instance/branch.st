accessing
branch
	^ branch ifNil: [ branch := self repository branch ]
