Extension { #name : #ClassVariable }

{ #category : #'*Ring-Core' }
ClassVariable >> asRingMinimalDefinitionIn: anRGEnvironment [ 

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		| def realClass |
		realClass := Smalltalk allClasses detect: [ :each | each classVariables includes: self ].
		def := RGClassVariable named: self key asSymbol parent: (realClass asRingMinimalDefinitionIn: anRGEnvironment).
		def ].
]
