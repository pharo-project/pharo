private
makeAvailable
	"This method must be the _only_ way to set a notNil value for the unique instance (current).  First, obtain a candidate instance and set current to a notNil placeholder (any notNil object not an instance of me would do;  this version uses false).  Next, check any subordinate resources needed by this resource.  Lastly, setUp the candidate and put it in current if it is available, ensuring that it is torn down otherwise."
	
	| candidate |
	current := nil.
	candidate := self new.
	self resources do: [:each | each availableFor: candidate].
	[candidate setUp.
	candidate isAvailable ifTrue: [current := candidate]]
		ensure: [current == candidate ifFalse: [candidate tearDown]]