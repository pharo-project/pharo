Extension { #name : 'CompiledMethod' }

{ #category : '*Epicea-Ring' }
CompiledMethod >> asEpiceaRingDefinition [

	^ (RGMethodDefinition named: self selector)
		  parentName: self methodClass name;
		  isMeta: self methodClass isMeta;
		  protocol: self protocolName;
		  sourceCode: self sourceCode;
		  stamp: self timeStamp;
		  package: (self package ifNotNil: [ :package | package name ]);
		  yourself
]
