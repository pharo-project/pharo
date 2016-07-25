private
taskClicked: aTask
	"A button for a task has been pressed.
	Close after selecting."
	
	self selectTask: aTask.
	self done