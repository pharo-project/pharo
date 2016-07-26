private
removeDangerouslyKey: key ifAbsent: aBlock
	"This is not really dangerous.  But if normal removal
	were done WHILE a MethodDict were being used, the
	system might crash.  So instead we make a copy, then do
	this operation (which is NOT dangerous in a copy that is
	not being used), and then use the copy after the removal."

	| index element |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	element := array at: index.
	array at: index put: nil.
	self basicAt: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ element