protocol
selectedCategories

	| associations |
	associations := self categoriesSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]