classification-rules
classifyInSuperclassProtocol: aMethod
	| currentClass |
	currentClass := aMethod methodClass.
	
	[ currentClass superclass isNil ] whileFalse: [
		currentClass := currentClass superclass.
		(currentClass includesSelector: aMethod selector) 
			ifTrue: [ 
				| possibleProtocol |
				possibleProtocol := (currentClass >> aMethod selector) protocol.
				((possibleProtocol beginsWith: '*') or: 
					(possibleProtocol asSymbol = currentClass package name))
						ifFalse: [ aMethod protocol: possibleProtocol.
								^ true ] 
							]].
	^ false.