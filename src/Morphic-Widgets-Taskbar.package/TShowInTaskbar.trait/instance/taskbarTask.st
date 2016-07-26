taskbar-private
taskbarTask

	^ TaskbarTask
		morph: self
		state: self taskbarState
		icon: self taskbarIcon
		label: self taskbarLabel