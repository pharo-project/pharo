tree
groupTreeModel
	^ groupTreeModel ifNil: [ groupTreeModel := self groupTreeModelClass new model: self ]