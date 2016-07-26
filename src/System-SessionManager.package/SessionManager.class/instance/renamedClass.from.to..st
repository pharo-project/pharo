registration
renamedClass: aClass from: oldName to: newName
	"Inform SessionManager that aClass has been renamed"
	
	categories do: [ :category |
		category renamedClass: aClass from: oldName to: newName ]