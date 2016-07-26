private
selectedItem
	selectedItem isNil
		ifTrue: [^ nil].
	^ selectedItem isSelected
		ifTrue: [ selectedItem]
		ifFalse: [ nil]