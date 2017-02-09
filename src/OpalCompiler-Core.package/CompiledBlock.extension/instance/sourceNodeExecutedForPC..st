*OpalCompiler-Core
sourceNodeExecutedForPC: aPC
	| blockNode |
	blockNode := self outerCode sourceNodeExecutedForPC: self pcInOuter.
	^blockNode sourceNodeExecutedForPC: aPC