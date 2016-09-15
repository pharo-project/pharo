accessing
default
	^ defaultInstance ifNil: [ defaultInstance := self new ]