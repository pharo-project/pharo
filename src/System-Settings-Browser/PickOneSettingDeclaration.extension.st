Extension { #name : 'PickOneSettingDeclaration' }

{ #category : '*System-Settings-Browser' }
PickOneSettingDeclaration >> inputWidget [
	| widget row |
	row := self theme newRowIn: self for: {
				widget := (self theme
						newDropListIn: self currentWorld
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
]

{ #category : '*System-Settings-Browser' }
PickOneSettingDeclaration >> maxNameWidthForFont: aFont [
	| max |
	max := 0.
	self domainValuesLabels
		do: [:v | max := max max: (aFont widthOfStringOrText: v)].
	^ max
]
