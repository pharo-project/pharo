accessing
searchStrategy: anObject
	"Can be #default, #rootsOnly or #allItems. Use #allItems ONLY if the tree have not an infinit number of children, else you will get an infinit loop."

	searchStrategy := anObject