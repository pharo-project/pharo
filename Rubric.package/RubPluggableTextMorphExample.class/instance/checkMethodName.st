events handling
checkMethodName
	| conf |
	conf := RubConfigurationChange new.
	self selectedClass
		ifNil: [ conf configurationBlock: [ :textField | textField textColor: Color red ] ]
		ifNotNil: [ 
			conf
				configurationBlock: [ :textField | 
					(self selectedClass canUnderstand: self selectedMethodName getString asSymbol)
						ifFalse: [ textField textColor: Color red ]
						ifTrue: [ textField textColor: Color black ] ] ].
	self selectedMethodName announce: conf