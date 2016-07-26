*GT-BytecodeDebugger
= anObject

	self == anObject
		ifTrue: [^ true].

	(self class == anObject class)
		ifFalse: [^ false].

	^ (self bytes = anObject bytes) and: [ 
		(self description = anObject description) and: [ 
			(self method = anObject method) and: [ 
				self offset = anObject offset ] ] ]