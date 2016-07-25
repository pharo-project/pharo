private selecting
explicitlySelectItem: anObject 
	(self selectedItem notNil and: [self selectedItem = anObject]) ifTrue: [ ^ self ].
	(self selectedItem isNil and: [ anObject isNil ]) ifTrue: [ ^ self ].
	anObject isNil ifTrue: [
		"we make sure to update the morph list by deselecting everything"
		self selectNodePath: nil.
		self selection: nil.].
	self roots do: [:eachRoot |
		eachRoot withContentsDo: [:each |
			each item = anObject ifTrue: [
				each expandParentPath.
				self selectNodePath: each path.
				^ self]]]