initialize - release
lazyIncrement
	^ lazyIncrement ifNil: [ lazyIncrement := 20 ] 