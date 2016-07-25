handling fingers
untrackFinger: anEvent
	| removedTracker |
	fingerCount := fingerCount - 1.
	removedTracker := fingers removeKey: anEvent fingerId ifAbsent: [ ^ self ].
	(devicesFingers at: anEvent deviceId) remove: removedTracker.