spec accessing
target
	^ self spec action ifNotNil: [:action | action receiver]
