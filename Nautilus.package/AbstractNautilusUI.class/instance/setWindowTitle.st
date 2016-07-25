displaying
setWindowTitle

	| title |
	shouldUpdateTitle ifFalse: [ ^ self ].
	title := self selectedClass 
				ifNil: [ self selectedPackage
							ifNil: [ self title ]
							ifNotNil: [: p | p name ]]
				ifNotNil: [:class |
					self selectedMethod
						ifNil: [ class name asString ]
						ifNotNil: [:method |
							String streamContents: [:s | s << method methodClass name asString<< '>>#'<< method selector ]]].
	self browsedEnvironment isSystem ifFalse: [ title := String streamContents: [:s | s << '--- ' << title << ' ---']].
	window title: title.