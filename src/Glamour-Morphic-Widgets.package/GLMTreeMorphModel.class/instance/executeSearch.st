search and filtering
executeSearch
	| newSelection check |
	newSelection := OrderedCollection new.
	roots do: [ :each | 
		check := self glamourPresentation searchStrategy value: self inputText value: each item.
		check ifTrue: [newSelection add: each path]].
	newSelection isEmpty 
		ifFalse: [ self selection: (MorphTreeMorphMultipleSelection new selectedNodePathList: newSelection) ]
		ifTrue: [self selection: nil]