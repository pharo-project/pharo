accessing
layoutForType: typeSymbol
	typeSymbol = #compiledMethod 
		ifTrue: [ ^ CompiledMethodLayout ].
	typeSymbol = #bytes 
		ifTrue: [ ^ ByteLayout ].
	typeSymbol = #words 
		ifTrue: [ ^ WordLayout ].
	typeSymbol = #weak 
		ifTrue: [ ^ WeakLayout ].
	typeSymbol = #variable 
		ifTrue: [ ^ VariableLayout ].
	typeSymbol = #normal 
		ifTrue: [ ^ FixedLayout ].
	typeSymbol = #immediate 
		ifTrue: [ ^ ImmediateLayout ].
	typeSymbol = #ephemeron
		ifTrue: [ ^ EphemeronLayout ].
	"hack to support user defined layouts"
	(Smalltalk hasClassNamed: typeSymbol) ifTrue: [ ^Smalltalk globals at: typeSymbol ].
	Error signal: 'Invalid layout type: ', typeSymbol asString.