accessing
roots: anArray
	roots := anArray collect:[:item| PluggableTreeItemNode with: item model: self].
	self list: roots.