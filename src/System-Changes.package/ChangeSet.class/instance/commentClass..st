class changes
commentClass: class 
	"Include indication that a class comment has been changed."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #comment