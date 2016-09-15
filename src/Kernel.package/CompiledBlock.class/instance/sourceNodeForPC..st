additions
sourceNodeForPC: aPC
	| blockNode |
	blockNode := self outerCode sourceNodeForPC: self pcInOuter.
	^blockNode sourceNodeForPC: aPC