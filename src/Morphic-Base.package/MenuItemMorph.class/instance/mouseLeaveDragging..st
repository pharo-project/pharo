events
mouseLeaveDragging: evt 
	"The mouse left the receiver. Do nothing if we're not in a 'valid menu transition', meaning that the current hand focus must be aimed at the owning menu."

	owner ifNil: [^self].
	evt hand mouseFocus == owner ifFalse: [^self].
	"If we have a submenu, make sure we've got some time to enter it before actually leaving the menu item"
	subMenu isNil 
		ifTrue: [owner selectItem: nil event: evt]
		ifFalse: 
			[self 
				addAlarm: #deselectTimeOut:
				with: evt
				after: 500]