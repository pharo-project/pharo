operations
ensureDeleteAllChildren
	"delete all children of the receiver and does not raise an error if the receiver does not exist"
	
	self exists ifFalse: [ ^ self  ].
	self deleteAllChildren