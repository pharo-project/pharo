announcement handling
monticelloVersionSaved: aMCVersionSaved 

	aMCVersionSaved repository description = 'cache' ifFalse: [
		self addEvent: aMCVersionSaved asEpiceaEvent ].
	"Note: Tried to encapsulate this check in the hierarchy of MCRepository 
	but caused troubles to iceberg, since it has a repository that's not in such hierarchy."
