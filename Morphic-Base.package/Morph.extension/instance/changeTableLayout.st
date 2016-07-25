*Morphic-Base
changeTableLayout
	| layout |
	((layout := self layoutPolicy) notNil and:[layout isTableLayout])
		ifTrue:[^self]. "already table layout"
	self layoutPolicy: TableLayout new.
	self layoutChanged.