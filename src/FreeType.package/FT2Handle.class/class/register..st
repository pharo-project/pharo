private-handle registry
register: aHandle
	(self registry includes: aHandle) ifTrue: [ ^ self ]. 
	self registry add: aHandle