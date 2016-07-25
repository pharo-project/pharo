operations
removeCategory: cat 
	"Remove the category named, cat. Create an error notificiation if the 
	category has any elements in it."

	| index lastStop |
	index := categoryArray indexOf: cat ifAbsent: [^self].
	lastStop := 
		index = 1
			ifTrue: [0]
			ifFalse: [categoryStops at: index - 1].
	(categoryStops at: index) - lastStop > 0 
		ifTrue: [^self error: 'cannot remove non-empty category ', cat].
	categoryArray := categoryArray copyReplaceFrom: index to: index with: Array new.
	categoryStops := categoryStops copyReplaceFrom: index to: index with: Array new.
	categoryArray isEmpty
		ifTrue:
			[categoryArray := Array with: Default.
			categoryStops := Array with: 0]
