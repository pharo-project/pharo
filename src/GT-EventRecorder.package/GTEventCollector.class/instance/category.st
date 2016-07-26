accessing
category
	^ category ifNil: [ category := self defaultCategory ]