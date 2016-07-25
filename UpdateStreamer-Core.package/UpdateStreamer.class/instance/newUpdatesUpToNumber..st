private - actions
newUpdatesUpToNumber: aNumber
	"Return a list of fully formed URLs of update files we do not yet have.  Look at the file 'updates.list'.
	No updates numbered higher than aNumber (if it is not nil) are returned " 

	"self new newUpdatesToNumber: 14000"

	self upToNumber: aNumber.
	^ self newUpdates.