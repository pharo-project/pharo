classification-rules
classifyInSuperclassProtocol: aMethod
	| currentClass |
	currentClass := aMethod methodClass.
	
	[ currentClass superclass isNil ] whileFalse: [
		currentClass := currentClass superclass.
		(currentClass includesSelector: aMethod selector)
			ifTrue: [ 
				aMethod protocol: (currentClass >> aMethod selector) protocol.
				^ true ]].
	
	^ false.