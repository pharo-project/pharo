preconditions
checkSuperMethods
	| superMessages nonSupers |
	nonSupers := OrderedCollection new.
	superMessages := OrderedCollection new.
	(selectors reject: [:each | self justSendsSuper: each]) do: 
			[:each | 
			(self superclassEquivalentlyDefines: each) 
				ifTrue: 
					[(class parseTreeFor: each) superMessages isEmpty 
						ifFalse: [superMessages add: each]]
				ifFalse: [nonSupers add: each]].
	nonSupers isEmpty & superMessages isEmpty ifTrue: [^self].
	self checkReferencesToAnyOf: nonSupers.
	self checkReferencesToSuperSendsToAnyOf: superMessages