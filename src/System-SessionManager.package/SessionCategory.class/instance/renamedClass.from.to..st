private
renamedClass: aClass from: oldName to: newName
	
	priorityLists do: [ :handlers |
		handlers 
			detect: [ :handler | handler handledId = oldName ]
			ifFound: [ :handler | handler registeredClassName: newName ] ]
