checkbox
disableUseRegEx
	useRegExCheckbox isSelected
		ifTrue: [ useRegExCheckbox toggleSelected].
	useRegExCheckbox enabled: false.
