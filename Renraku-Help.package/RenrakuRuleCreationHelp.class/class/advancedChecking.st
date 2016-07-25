pages
advancedChecking

	^ HelpTopic
		title: 'Advanced Checking'
		contents: 
'While there is a default implementation which relies on #basicCheck: and creates an instance of ReTrivialCritique, it is advised do override the #check:forCritiquesDo: method. This method accepts an entity and a block which could be evaluated for each detected critique. This means that one rule can detect multiple critiques about one entity. For example if a rule checks for unused variables it can report all of them with a dedicated critique for each.

The block which should be evaluated for each critique may accept one argument: the critique object, this is why you have to evaluate it with #cull:. You may use the #critiqueFor: method if you don''t feel comfortable with critiques yes. For example:

    self critiqueFor: anEntity

will return ReTrivialCritique about the entity. Later you can update your code to create other kinds of critiques more suitable for your case.'