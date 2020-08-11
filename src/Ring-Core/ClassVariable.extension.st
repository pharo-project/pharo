Extension { #name : #ClassVariable }

{ #category : #'*Ring-Core' }
ClassVariable >> asRingMinimalDefinitionIn: anRGEnvironment [ 

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
			RGClassVariable 
				named: self key asSymbol 
				parent: (self definingClass asRingMinimalDefinitionIn: anRGEnvironment) ]
]
