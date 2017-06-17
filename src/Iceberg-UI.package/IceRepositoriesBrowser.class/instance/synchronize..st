private utilities
synchronize: repository
	"This hack is a workaround because port #strongSelection of glamour generates multiple events for a single double-click."
	(lastSynchronization isNil 
		or: [ (DateAndTime current - lastSynchronization first) seconds > 1
		or: [ lastSynchronization second  ~= repository ]])
		ifTrue: [ 
			lastSynchronization := { DateAndTime current. repository }.
			IceGlamourSynchronizer synchronize: repository ]
	