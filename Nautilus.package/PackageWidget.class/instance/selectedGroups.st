protocol
selectedGroups

	| associations |
	associations := groupsSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]