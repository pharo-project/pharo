private-extensions-methodcontext
fuelAcceptForMethodContext: aGeneralMapper
	<flExtensionOf: #MethodContext selector: #fuelAccept:>
	^aGeneralMapper visitMethodContext: self