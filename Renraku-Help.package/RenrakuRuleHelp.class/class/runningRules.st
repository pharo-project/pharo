pages
runningRules

	^ HelpTopic
		title: 'Running Rules'
		contents: 'Usually you don''t have to run rules yourself, this is done by tools. But it is a good idea to be familiar with the API available in rules for developing your own tools and dubugging unexpected behaviour.
		
First of all tools traverse the hierarchy of rule classes and selects the required ones buy testing them with #checksMethod, #checksClass, etc…

The main method that the rules implement to check entities is #check:forCritiquesDo:. It accepts the entity to be checked as the first argument, and the block to be evaluated for each critique. The block may accept one parameter which is a critique object. For example:

	rule
		check: anEntity
		forCritiquesDo: [ :critique |
			"do something for each critique" ]
		
		
There are also 3 convenience methods:

#check:
Accepts an entity to check and returns the collection of critiques.

#check:ifNoCritiques:
Similarly to previous one checks an entity and returns the collection of critiques. Additionally accepts a block that will be evaluated if no critiques are found.

#check:forCritiquesDo:ifNone:
Similarly to #check:forCritiquesDo: checks an entity and evaluates the block for each detected critique. Additionally accepts a block that will be evaluated if no critiques are found.'