streams
streamWritable: writable do: aBlock
	^ writable 
		ifTrue: [ self writeStreamDo: aBlock ]
		ifFalse: [ self readStreamDo: aBlock ]