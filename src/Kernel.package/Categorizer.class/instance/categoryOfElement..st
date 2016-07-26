queries
categoryOfElement: element 
	"Answer the category associated with the argument, element."

	| index |
	index := self numberOfCategoryOfElement: element.
	^ index = 0
		ifTrue: [ nil]
		ifFalse: [ categoryArray at: index ]