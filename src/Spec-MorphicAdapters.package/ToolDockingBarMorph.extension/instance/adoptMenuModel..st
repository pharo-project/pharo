*Spec-MorphicAdapters
adoptMenuModel: aMenuModel

	aMenuModel menuGroups doWithIndex: [ :aGroup :index | 
		self 
			adoptMenuGroupModel: aGroup
			first: (index = 1) ]