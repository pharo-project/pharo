layout-menu
changeNoLayout
	self layoutPolicy ifNil:[^self]. "already no layout"
	self layoutPolicy: nil.
	self layoutChanged.