private
removedMethod: string with: aChunk
	
	| orClass tokens |
	tokens := self scannedSource: string.
	tokens size >= 3
		ifTrue:[ orClass := self classNamed: tokens first. 
		^ (tokens second == #class
			ifTrue: [ orClass theMetaClass ]
			ifFalse:[ orClass ]) removeSelector: tokens last ].
	doIts add: aChunk