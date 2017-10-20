private - updating
updateMappings
	"Update the shapes of the joins."
	
	self mappings do: [:j |
		j width: self width]