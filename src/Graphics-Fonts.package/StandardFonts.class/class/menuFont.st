font accessing
menuFont
	^ MenuFont ifNil: [MenuFont := self defaultFont]