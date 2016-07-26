instance creation
new

	self isPluginAvailable 
		ifTrue: [ ^self basicNew ]
		ifFalse: [ ^MD5NonPrimitive basicNew ]