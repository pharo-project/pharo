accessing
contents
	| list workingCopies |
	workingCopies := model unsortedWorkingCopies.
	list := item requiredPackages collect: 
					[:each | 
					workingCopies detect: [:wc | wc package = each] ifNone: [nil]]
				thenSelect: [:x | x notNil].
	^list collect: [:each | self class with: each model: model]