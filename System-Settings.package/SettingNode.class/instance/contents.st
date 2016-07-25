accessing
contents
	(self settingDeclaration hasValue
			and: [self settingValue = false])
		ifTrue: [^ OrderedCollection new]
		ifFalse: [^ contents 
				ifNil: [contents := self allChildren.
					self settingDeclaration ordering
						ifTrue: [contents := contents asArray sort: self model sortBlock].
					contents := self model retainedNodesFromList: contents asOrderedCollection]]