loading
load
	"It loads all instances of StoredSetting from a stream."
	stream ifNil: [ ^ #() ].
	stream atEnd ifTrue: [ ^ #() ].
	[ | reader | 
		reader := STON reader on: stream; yourself.
		storedSettings := reader next.
	] ensure: [ stream close ].
	^ storedSettings