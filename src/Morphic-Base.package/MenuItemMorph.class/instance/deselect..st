selecting
deselect: evt
	self isSelected: false.
	subMenu ifNotNil: [
		owner ifNotNil:[owner activeSubmenu: nil].
		self removeAlarm: #deselectTimeOut:].