icons-actions
addActionsToIcon

	closeIcon 
		on: #mouseEnter send: #mouseEnterIcon to: self;
		on: #click send: #close to: self;
		on: #mouseLeave send: #mouseLeaveIcon to: self