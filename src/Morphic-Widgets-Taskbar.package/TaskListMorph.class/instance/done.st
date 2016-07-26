running
done
	"Close the tasklist and make the active task current."
	
	self delete.
	self activeTask ifNotNil: [ :t | t activate ]