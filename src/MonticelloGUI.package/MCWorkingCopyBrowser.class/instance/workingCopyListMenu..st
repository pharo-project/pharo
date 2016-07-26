morphic ui
workingCopyListMenu: aMenu

	workingCopy ifNil: [ ^ aMenu ].
	(workingCopy package name beginsWith: 'ConfigurationOf') ifTrue: [
		self fillMenu: aMenu fromSpecs: self collectConfigurationMenuSpecs.
		aMenu addLine ].
	self fillMenu: aMenu fromSpecs: self historyMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self packageMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self revertPackageMenuSpec.
	aMenu addLine.
	self fillMenu: aMenu fromSpecs: self unloadPackageMenuSpec.
	aMenu addLine.
	1 to: self orderSpecs size do: [ :index | 
		aMenu
			addUpdating: #orderString:
			target: self
			selector: #order:
			argumentList: {index} ].
	^ aMenu 