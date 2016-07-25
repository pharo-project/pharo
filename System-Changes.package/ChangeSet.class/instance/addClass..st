change logging
addClass: class 
	"Include indication that a new class was created."

	class wantsChangeSetLogging ifFalse: [^ self].
	self atClass: class add: #new.
	self atClass: class add: #change.
	self addCoherency: class name