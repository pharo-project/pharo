private
initializeStream

	self isStandardType 
		ifTrue: [ fileStream := self standardStream ]
		ifFalse: [ super initializeStream ]