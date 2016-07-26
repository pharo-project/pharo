*Polymorph-Widgets
isRestored
	"Answer whether we are neither expanded or collapsed."

	^(self isMinimized or: [self isMaximized]) not