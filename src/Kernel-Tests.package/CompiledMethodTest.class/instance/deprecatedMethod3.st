examples
deprecatedMethod3
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self
		deprecated: 'Example of a deprecated method with transform'
		transformWith: '`@receiver deprecatedMethod3' 
						-> '`@receiver deprecatedMethod3'.
