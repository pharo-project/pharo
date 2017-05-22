accessing
representationType
	^ representationType ifNil: [ representationType := self defaultRepresentationType ]