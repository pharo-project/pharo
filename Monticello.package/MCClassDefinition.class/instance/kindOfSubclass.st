printing
kindOfSubclass
	type = #normal ifTrue: [^ ' subclass: '].
	type = #words ifTrue: [^ ' variableWordSubclass: '].
	type = #variable ifTrue: [^ ' variableSubclass: '].
	type = #bytes ifTrue: [^ ' variableByteSubclass: '].
	type = #weak ifTrue: [^ ' weakSubclass: ' ].
	type = #compiledMethod ifTrue: [^ ' variableByteSubclass: ' ].
	type = #immediate ifTrue: [ ^ ' immediateSubclass: ' ].
	type = #ephemeron ifTrue: [ ^ ' ephemeronSubclass: ' ].
	"hack to support user defined layouts"
	(Smalltalk hasClassNamed: type) ifTrue: [ ^type asString].
	self error: 'Unrecognized class type'