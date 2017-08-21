testing method dictionary
isDisabledSelector: selector
	^ self classAndMethodFor: selector do: [:c :m | m isDisabled] ifAbsent: [false]