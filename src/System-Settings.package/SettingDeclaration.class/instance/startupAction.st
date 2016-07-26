export
startupAction
	"I assume here that the receiver is always a class"

	 | targetSymbol currentValue |
			
	targetSymbol := self target isSymbol
						ifTrue: [ self target ]
						ifFalse: [ self target name asSymbol ].
						
	currentValue := self targetSelector
						ifNil: [ (Smalltalk at: targetSymbol) perform: self getSelector ]
						ifNotNil: [:s | ((Smalltalk at: targetSymbol) perform: s) perform: self getSelector].
	^ StartupAction 
		name: self label 
		code: (String streamContents: [:s |
					self targetSelector ifNotNil: [ s << '(' ].
					s << '(Smalltalk at: '.
					targetSymbol asSymbol printOn: s.
					self targetSelector ifNotNil: [:ts | s << ') perform: '. ts printOn: s ].
					s << ') perform: '.
					self setSelector asSymbol printOn: s.
					s << ' with: ('.
					currentValue settingStoreOn: s.
					s <<')' ])
		runOnce: true