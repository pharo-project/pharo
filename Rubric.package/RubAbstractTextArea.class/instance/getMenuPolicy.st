menu
getMenuPolicy
	^ getMenuPolicy ifNil: [ getMenuPolicy := self defaultGetMenuPolicy ]