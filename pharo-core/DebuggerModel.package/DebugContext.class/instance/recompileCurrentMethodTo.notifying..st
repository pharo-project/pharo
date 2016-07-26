evaluating actions
recompileCurrentMethodTo: aText notifying: aNotifyer
	| classOrTraitOfMethod selector |
	
	selector := self selectedClass compiler parseSelector: aText.
	(self checkSelectorUnchanged: selector) ifFalse: [ ^ nil ].
	classOrTraitOfMethod := self confirmOnTraitOverwrite: selector inClass: self selectedClass.
	
	classOrTraitOfMethod ifNil:[ ^ nil].
		
	selector := classOrTraitOfMethod 
		compile: aText 
		classified: self selectedMessageCategoryName 
		notifying: aNotifyer.
	^ selector 
		ifNotNil: [ classOrTraitOfMethod compiledMethodAt: selector ]
		