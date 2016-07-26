updating
enabledFromModel
	| val |
	self model ifNil: [ ^ self ].
	val := self model enabled.
	val ~= self enabled
		ifTrue: [ self enabled: val ] 