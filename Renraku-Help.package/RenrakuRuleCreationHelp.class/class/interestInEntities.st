pages
interestInEntities

	^ HelpTopic
		title: 'Specifying an Interest in Entities'
		contents: 
'The class-side methods #checksMethod, #checksClass, #checksPackage and #checksNode return true is the rule checks methods, classes or traits, packages and AST nodes respectively. Tools will pass entities of the specified type to the rule for checking. A rule may check multiple types of entities but please avoid checks for types inside of rules. E.g. if a rule checks whether an entity is named with a swearing word and does this by obtaining the name of the entity and matching substrings. It is completely fine to specify that the rule checks classes and packages as you don''t have to distinguish which entity was passed to you.'