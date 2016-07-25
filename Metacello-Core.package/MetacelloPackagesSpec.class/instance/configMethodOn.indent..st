printing
configMethodOn: aStream indent: indent

	| packageSpecs |
	packageSpecs := self map values.
	packageSpecs size = 0 ifTrue: [ ^aStream nextPutAll: 'spec add: []' ].
	packageSpecs size = 1
		ifTrue: [
			aStream 
				tab: indent; 
				nextPutAll: 'spec add: ['; cr.
			packageSpecs first configMethodOn: aStream indent: indent + 1.
			aStream nextPut: $]; cr ]
		ifFalse: [
			aStream 
				tab: indent; 
				nextPutAll: 'spec'.
			1 to: packageSpecs size do: [:index | | packageSpec |
				packageSpec := packageSpecs at: index.
				aStream 
					tab: indent + 1;
					nextPutAll: 'add: ['; cr.
				packageSpec configMethodOn: aStream indent: indent + 2.
				aStream nextPut: $].
				index < packageSpecs size
					ifTrue: [ aStream nextPut: $; ].
				aStream cr ]]