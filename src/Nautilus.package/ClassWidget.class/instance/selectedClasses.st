protocol
selectedClasses

	| associations |
	associations := self classesSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]