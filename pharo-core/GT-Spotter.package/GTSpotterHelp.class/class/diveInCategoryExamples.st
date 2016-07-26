documentation
diveInCategoryExamples
	^ HelpTopic 
		title: 'Dive-in Category Examples'
		contents:
'Dive-in (Meta+ArrowRight) allows you to go inside one element. Dive-in-category allows you to refine the search for the current category.

Let''s look at an example.

Suppose you want to search for all classes that have both Spotter and Morph in their names. Essentially, this is a search that involves two queries connected through an logical AND operation. 

So, you first search for:
	Spotter #classes

Then you dive-in category (Meta+Shift+ArrowRight, or click on the arrow button next to #Classes category name). This will get you in another step that contains only the elements from the collection. Then you refine the search:
	Morph
	
... and you get the classes that have both Spotter and Morph in their name. 
'