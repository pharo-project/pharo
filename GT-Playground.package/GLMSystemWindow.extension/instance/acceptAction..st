*GT-Playground
acceptAction: anAction 
	"Be compatible with edit:label:accept:.
	Not the best place for this method.
	Interim situation to move things forward..."
	self flag: #TODO.
	self model ifNotNil: [:aModel | aModel acceptAction: anAction].