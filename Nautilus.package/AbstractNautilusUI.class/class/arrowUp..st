icons behavior
arrowUp: aMethod
	| newMethod |
	newMethod := aMethod methodClass superclass lookupSelector: aMethod selector.
	newMethod
		ifNil: [ MethodWidget removeAllFromMethodsIconsCache:  aMethod ]
		ifNotNil: [ Nautilus openOnMethod: newMethod ]