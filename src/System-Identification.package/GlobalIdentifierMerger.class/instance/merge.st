actions
merge
	"If UUIDs are different, it is likely different user and we should use
	localy stored settings. If the UUIDs are same, we keep settings of the image.
	
	It returns true if existing dictionary is changed."
	^ ((existing at: #computerUUID ifAbsent: nil) = (stored at: #computerUUID ifAbsent: nil)
		or: [ (stored includesKey: #computerUUID) not ])
			ifTrue: [ self mergeSameComputer ]
			ifFalse: [ self mergeDifferentComputer ]