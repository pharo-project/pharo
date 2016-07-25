updating
updateStatus: aBoolean
	"Update the status display, at most once a second if aBoolean is true."

	(aBoolean and: [ lastUpdate = Time totalSeconds ])
		ifTrue: [ ^ self ].
	self changed: #statusText; changed: #statusColor.
	lastUpdate := Time totalSeconds.