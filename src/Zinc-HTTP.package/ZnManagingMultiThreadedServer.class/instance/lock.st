accessing
lock
	^ lock ifNil: [ lock := Mutex new ]