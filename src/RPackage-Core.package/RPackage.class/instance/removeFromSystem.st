removing
removeFromSystem
	
	self definedClasses do: #removeFromSystem.
	self extensionMethods do: #removeFromSystem.
	self unregister.