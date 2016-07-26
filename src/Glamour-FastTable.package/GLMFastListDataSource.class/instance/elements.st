accessing
elements
	^ elements ifNil: [ elements := self calculateElements ]