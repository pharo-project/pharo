actions
listConfigurationDetails
	"List possible configurations of the given repository"
	
	self printMetacelloProjectDetails: self project.
	
	^ self exitSuccess