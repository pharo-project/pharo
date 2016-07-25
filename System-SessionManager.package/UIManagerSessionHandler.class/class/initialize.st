initialize-release
initialize
	"has to be executed at the end of the GUI category"
	SessionManager default 
		register: self uniqueInstance
		inCategory: SessionManager default guiCategory 
		atPriority: SmallInteger maxVal