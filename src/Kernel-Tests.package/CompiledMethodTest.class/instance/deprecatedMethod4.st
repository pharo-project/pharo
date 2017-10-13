examples
deprecatedMethod4
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self
	
		deprecated: 'Example of a deprecated method with transform'
		on: '01/01/1970'
		in: #Pharo6
		transformWith: '`@receiver deprecatedMethod4' 
						-> '`@receiver deprecatedMethod4'.
