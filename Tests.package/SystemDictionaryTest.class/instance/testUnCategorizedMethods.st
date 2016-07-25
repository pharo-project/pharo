problems
testUnCategorizedMethods
	| categories slips  |	categories := self categoriesForClass: self targetClass.
	slips := categories select: [:each | each = #'as yet unclassified'].
	self should: [slips isEmpty].	