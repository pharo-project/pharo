"
I am a cluster for CompiledBlock instances.

"
Class {
	#name : #FLCompiledBlockCluster,
	#superclass : #FLCompiledCodeCluster,
	#category : #'Fuel-Core-Clusters'
}

{ #category : #analyzing }
FLCompiledBlockCluster class >> clusterBucketIn: aClusterization [

	^aClusterization primitivesBucket
]

{ #category : #'serialize/materialize' }
FLCompiledBlockCluster >> materializeInstanceWith: aDecoder [

	| compiledBlock header size |

	header := aDecoder nextEncodedInt64.
	size := aDecoder nextEncodedUint16.

	compiledBlock := CompiledBlock newMethod: size header: header.

	compiledBlock initialPC to: compiledBlock size
		do: [ :index | compiledBlock at: index put: aDecoder nextEncodedByte ].
		
	^ compiledBlock
]

{ #category : #'serialize/materialize' }
FLCompiledBlockCluster >> serializeInstance: aCompiledBlock with: anEncoder [
	| header size cmSize cmInitialPC |

	header := aCompiledBlock header.
	cmSize := aCompiledBlock size.
	cmInitialPC := aCompiledBlock initialPC.
	size := cmSize - cmInitialPC + 1.

	anEncoder 
		encodeInt64: header; "Signed int as of Pharo 40"
		encodeUint16: size.	

	cmInitialPC to: cmSize
		do: [ :index | anEncoder encodeByte: (aCompiledBlock at: index) ]
]
