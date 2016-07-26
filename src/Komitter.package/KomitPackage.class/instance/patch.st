accessing
patch
	^ patch ifNil: [ patch := self package patch ]