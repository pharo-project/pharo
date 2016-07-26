running
redefined: aClass with: instanceVariableNames

	| name |
	name := aClass name.
	self removeFromSystem: aClass.
	"In this case we need to create a class with notifications"
	^ (self withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames) renameSilently: name.
	