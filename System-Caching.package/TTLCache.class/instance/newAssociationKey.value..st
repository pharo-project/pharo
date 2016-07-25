private 
newAssociationKey: key value: value
	"Override this method in a subclass to customise expiration behavior"
	
	^ TTLAssociation key: key value: value