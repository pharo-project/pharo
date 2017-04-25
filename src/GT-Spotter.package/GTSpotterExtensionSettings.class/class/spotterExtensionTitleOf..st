settings
spotterExtensionTitleOf: extensionAsCompiledMethod
	"
	Return an approximation of a description of the extension provided as argument.
	The argument has to be a compiled method representing a Spotter extension
	
	The description is fetched from the argument passed to the list processor.
	For example:
	self spotterExtensionTitleOf: MCPackage>> #spotterForPackageFor: 
	"
	| title |
	title := extensionAsCompiledMethod ast allChildren 
		detect: [ :node | node isMessage and: [ node selector = #title: ] ]
		ifFound: [ :node | node arguments first value. ]
		ifNone: [ extensionAsCompiledMethod selector ].
	title isString ifFalse: [ title := extensionAsCompiledMethod selector ].
	^ title