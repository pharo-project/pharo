dependents
addDependent: aDependent
	"Ensure to only have one dependent at every moment"
	
	dependents := #().
	super addDependent: aDependent