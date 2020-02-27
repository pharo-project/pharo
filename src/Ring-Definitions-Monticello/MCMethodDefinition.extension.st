Extension { #name : #MCMethodDefinition }

{ #category : #'*Ring-Definitions-Monticello' }
MCMethodDefinition >> asRingDefinition [

	^RGMethodDefinition new
		name: self selector;
		parentName: self className;
		isMetaSide: self classIsMeta;
		protocol: self protocol;
		sourceCode: self source;    
		stamp: self timeStamp
]
