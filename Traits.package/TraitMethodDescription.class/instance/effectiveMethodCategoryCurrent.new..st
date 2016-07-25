accessing
effectiveMethodCategoryCurrent: currentCategoryOrNil new: newCategoryOrNil
	| result size isCurrent isConflict |
	size := self size.
	size = 0 ifTrue: [^ nil].
	result := self locatedMethods anyOne category.
	size = 1 ifTrue: [^ result].
	
	isCurrent := currentCategoryOrNil isNil.
	isConflict := false.
	self locatedMethods do: [:each | | cat |
		cat := each category.
		isCurrent := isCurrent or: [cat == currentCategoryOrNil].
		isConflict := isConflict or: [cat ~~ result]].
	isConflict ifFalse: [^ result].
	(isCurrent not and: [newCategoryOrNil notNil]) ifTrue: [^ newCategoryOrNil].
	
	^ Protocol ambiguous 