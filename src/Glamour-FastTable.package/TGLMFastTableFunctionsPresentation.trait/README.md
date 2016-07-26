I am a Trait that manage the differents functionnalities for a FastTable related presentation.

Description
--------------------

My users have to define a #parameters method that need to be a getter for a Dictionary. Then I will store the informations I manage into this dictionary.

I work with all the presentations that use a FastTable as GLMFastLastPresentation, GLMFastTablePresentation or GLMFastTreePresentation.

Public API and Key Messages
--------------------

- #enableFilter 		add a filter to the GLMFastTable. (not compatible with the search)

- #enableSearch 		add a search to the GLMFastTable (A filter will remove the elements that doesn't match when a search will just highlight the matching results).
		
- #enableFilterWithAction: aBlock / #enableFilterWithAction:  aBlock named: aString 		add a filter and an action button. The action is define by a block that can take 3 parameters (the dataSource with the currents elements, the current filter pattern and the selected row of the table)
		
#searchOn: and #filterOn: are not implemented yet.

Example
--------------------

	aGLMFastTablePresentation enableFilterWithAction: [ :dataSource :pattern :item | item browse ] named: 'Browse'. 

Internal Representation and Key Implementation Points.
--------------------

To remember the options I keep some values inside a Dictionary. This dictionary is manage by a presentation and I get it via the #parameters method.