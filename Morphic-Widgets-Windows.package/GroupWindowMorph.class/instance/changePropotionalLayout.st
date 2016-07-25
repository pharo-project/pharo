layout
changePropotionalLayout
	| layout |
	((layout := self layoutPolicy) notNil and:[layout isProportionalLayout])
		ifTrue:[^self]. "already proportional layout"
	self layoutPolicy: ProportionalLayout new.
	self layoutChanged.