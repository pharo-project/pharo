accessing
realm
	^ realm ifNil: [ realm := self class defaultRealm ]