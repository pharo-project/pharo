Extension { #name : #CompiledMethod }

{ #category : #'*Epicea-Ring' }
CompiledMethod >> asEpiceaRingDefinition [

	^ (RGMethodDefinition named: self selector)
		parentName: self methodClass name;
		isMetaSide: self methodClass isMeta;
		protocol: self category;
		sourceCode: self sourceCode;
		stamp: self timeStamp;
		package: (self package ifNotNil: [:package | package name]);
		yourself.

]
