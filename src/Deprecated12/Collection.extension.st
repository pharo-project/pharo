Extension { #name : 'Collection' }

{ #category : '*Deprecated12' }
Collection class >> <- anArray [

	self
		deprecated: 'Use newFromArray: instead of current one'
		on: '16 February 2024'
		in:
		'Pharo-12.0.0+SNAPSHOT.build.1386.sha.268778ce1964491fa36c97ec014bea569f09c4d3 (64 Bit)'.

	^ self newFromArray: anArray
]
