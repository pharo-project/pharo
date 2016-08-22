order
isActionHandled
	"Return true if the provided method fits this action requirement. Implementation note: The best way to make sure that the system will break when we will add a new action is to use self subclassResponsibility for this method. So by default return false to avoid blowing up the system under your fingers!"
	^ false