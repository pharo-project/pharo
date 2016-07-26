operations
deleteAllChildren
	"delete all children of the receiver, raise an error if the receiver does not exist"
	
	self children do: [:aReference | aReference deleteAll ]