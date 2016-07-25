taskbar-private
taskbarButtonFor: aTaskbar
	"Answer a new task bar button for the receiver.
	Answer nil if not required."
	
	| taskbarButton |
	
	taskbarButton := aTaskbar theme
		newBasicTaskbarButtonIn: aTaskbar
		for: self.
		
	self customizeTaskbarButton: taskbarButton.
	
	^ taskbarButton