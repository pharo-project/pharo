accessing
fromInteger: anIntegerValue
	| theItem |
	theItem := self itemAt: anIntegerValue.
	theItem ifNil: [ self error: 'Invalid value for ' , self name , ' enumeration' ].
	^ self at: theItem