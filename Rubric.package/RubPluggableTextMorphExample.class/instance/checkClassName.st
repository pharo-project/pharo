events handling
checkClassName
	| conf |
	conf := RubConfigurationChange new.
	self selectedClass
		ifNil: [ conf configurationBlock: [ :textField | textField textColor: Color red ] ]
		ifNotNil: [ conf configurationBlock: [ :textField | textField textColor: Color black ] ].
	self selectedClassName announce: conf