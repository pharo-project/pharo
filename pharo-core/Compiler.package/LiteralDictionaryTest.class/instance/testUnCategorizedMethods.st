problems
testUnCategorizedMethods
"this test doesn't pass :"
	| categories slips  |
	categories := self categoriesForClass: self targetClass.
	slips := categories select: [:each | each = #'as yet unclassified'].
	self should: [slips isEmpty].	