private
duplicate: aClass name: aSymbol deep: aBoolean
	| superclass superclassName name class |
	(self model includesClassNamed: aSymbol)
		ifTrue: [ ^ nil ].
	superclass := aClass superclass 
		ifNil: [ self rootClass ].
	superclassName := (self model includesClassNamed: superclass name)
		ifFalse: [ superclass name ]
		ifTrue: [ 
			(name := self execute: superclass name) = superclass name
				ifFalse: [ self duplicate: superclass name: name deep: aBoolean ].
			name ].
	self model defineClass: ('<1s>
		subclass: #<2s>
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: <3p>' 
			expandMacrosWith: superclassName 
			with: aSymbol with: aClass category asString).
	aBoolean ifTrue: [
		(class := self model classNamed: aSymbol) 
			ifNil: [ ^ self ].
		self copyFrom: aClass to: class.
		self copyFrom: aClass theMetaClass to: class theMetaClass ].
	^ nil