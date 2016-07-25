initialize-release
removeFromSystem: logged
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	
	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to deactivate and unload itself-- two separate events in the module system"
	self release.
	self unload.
	self traitUsers do: [ :user | user removeFromComposition: self ].
	self superclass ifNotNil:
		["If we have no superclass there's nothing to be remembered"
		self superclass addObsoleteSubclass: self].
	self environment forgetClass: self logged: logged.
	self obsolete.