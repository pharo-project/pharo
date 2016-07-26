initialize-release
readTargetClassFrom: aPragma argumentNames: argumentNames argumentValues: argumentValues
	| class |
	class := Smalltalk
		at: (argumentValues at: (argumentNames indexOf: 'flExtensionOf:'))
		ifAbsent: [ ^ nil ].
		
	^ (aPragma method pragmas anySatisfy: [ :pragma |
		pragma key = #flExtensionClassSide ])
			ifTrue: [ class class ]
			ifFalse: [ class ]