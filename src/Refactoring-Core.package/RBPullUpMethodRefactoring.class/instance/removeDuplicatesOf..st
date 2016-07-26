transforming
removeDuplicatesOf: aSelector 
	| tree |
	tree := class superclass parseTreeFor: aSelector.
	class superclass allSubclasses do: 
			[:each | 
			((each directlyDefinesMethod: aSelector) and: 
					[(tree equalTo: (each parseTreeFor: aSelector) exceptForVariables: #()) 
						and: [(each superclass whoDefinesMethod: aSelector) == class superclass]]) 
				ifTrue: 
					[removeDuplicates 
						ifFalse: 
							[removeDuplicates := 
							self 
								refactoringConfirmWarning: 'Do you want to remove duplicate subclass methods?'].
					removeDuplicates ifTrue:[
					each removeMethod: aSelector]]]