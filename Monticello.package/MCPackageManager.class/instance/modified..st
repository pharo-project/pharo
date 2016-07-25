accessing
modified: aBoolean
	modified = aBoolean
		ifTrue: [ ^ self ].
	modified := aBoolean.
	self changed: #modified.
	modified
		ifFalse: [ 
			ChangesLog default logChange: '"' , self packageName , '"' ].
	self announcer announce: (MCPackageModified package: self package)
	