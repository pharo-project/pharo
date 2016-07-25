events
activateSubmenu: evt
	"Activate our submenu; e.g., pass control to it"
	subMenu ifNil:[^false]. "not applicable"
	(subMenu fullContainsPoint: evt position) ifFalse:[^false].
	subMenu activate: evt.
	self removeAlarm: #deselectTimeOut:.
	^true