user interface
inputWidget
	| widget row |
	row := self theme newRowIn: World for: {
				widget := (self theme 
						newDropListIn: World
						for: self
						list: #domainValuesLabels
						getSelected: #index
						setSelected: #index:
						getEnabled: #enabled
						useIndex: true
						help: self description)
						extent: 1 @ 30}.
	widget hResizing: #rigid.
	widget width: (self maxNameWidthForFont: widget font) + 50.
	^ row