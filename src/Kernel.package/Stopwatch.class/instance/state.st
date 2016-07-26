accessing
state

	^ state ifNil: [ state := #suspended ]
