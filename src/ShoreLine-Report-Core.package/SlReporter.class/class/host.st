settings
host

	^ host ifNil: [ host := self defaultHost ]