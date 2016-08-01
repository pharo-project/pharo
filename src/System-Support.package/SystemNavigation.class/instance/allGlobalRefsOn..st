private
allGlobalRefsOn: aSymbol

	| binding|
	
	binding := self environment bindingOf: aSymbol .
	binding ifNil: [ ^ #() ].
	
	^ self allReferencesTo: binding	
	