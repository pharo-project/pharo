private-changes
removeMethod: aSelector from: aRBClass 
	self flushCaches.
	^changes removeMethod: aSelector from: aRBClass