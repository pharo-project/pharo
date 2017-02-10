accessing
log

	^ log ifNil: [ log := self newLog ]