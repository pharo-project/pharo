luggableListItemWrapper makes it more easy for clients to use hierarchical lists. Rather than having to write a subclass of ListItemWrapper, a PluggableListItemWrapper can be used to provide the appropriate information straight from the model:
	string - an explicit string representation (contrary to the 'item' which contains any kind of object)
	getStringSelector - a message invoked to retrieve the sting representation of its item dynamically from its model (when a constant representation is undesirable)
	hasContentsSelector - a message invoked in the model to answer whether the item has any children or not.
	getContentsSelector - a message invoked in the model to retrieve the contents for its item.

All callback selectors can have zero, one or two arguments with the item and the wrapper as first and second argument.