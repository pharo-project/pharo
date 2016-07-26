public
instance
	^ instance ifNil: [ instance := self basicNew initialize start ]