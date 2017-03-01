removing
removeFromSystem
	| extendedClasses |

	extendedClasses := self extendedClasses.
	
	self definedClasses do: #removeFromSystem.
	self extensionMethods do: #removeFromSystem.

	extendedClasses do:[:each | each organization removeProtocolIfEmpty: ('*' , name) asSymbol].
	self unregister.