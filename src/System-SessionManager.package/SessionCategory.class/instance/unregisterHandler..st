registration
unregisterHandler: anHandlerId 
	priorityLists values
		do: [ :handlers |
				handlers 
					detect: [ :handler | handler handledId = anHandlerId ]
					ifFound: [ :handler | handlers remove: handler ] ]