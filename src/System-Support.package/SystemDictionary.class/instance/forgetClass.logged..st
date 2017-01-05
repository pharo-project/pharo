classes and traits
forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	self organization removeElement: aClass name.
	SessionManager default unregisterClassNamed: aClass name.
	self removeKey: aClass name ifAbsent: [].
	self flushClassNameCache