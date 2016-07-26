morphic
newListMorphs
	| list |
	self model requests do: [:each | 
		list := (GLMMorphicSingleSpotter on: each parent: self).
		self childrenMorphs add: list ].
	^ childrenMorphs collect: #newListMorph