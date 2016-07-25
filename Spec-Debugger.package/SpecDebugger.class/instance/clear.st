actions
clear
	
	self removeActionsForSession: self session.
	"No more code should follow after this, as the session might terminate this process."
	self session ifNotNil: [ :aSession | aSession terminate]