export
exportSettingAction
	| targetSymbol currentValue |
	
	targetSymbol := self target isSymbol
						ifTrue: [ self target ]
						ifFalse: [ self target name asSymbol ].
						
	currentValue := self targetSelector
						ifNil: [ (Smalltalk at: targetSymbol) perform: self getSelector ]
						ifNotNil: [:s | ((Smalltalk at: targetSymbol) perform: s) perform: self getSelector].
	
	^ (self hasDefault not or: [ (self default = currentValue) not ])
		ifTrue: [ self startupAction ]
		ifFalse: [ "We do nothing" nil ]