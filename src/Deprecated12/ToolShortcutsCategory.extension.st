Extension { #name : 'ToolShortcutsCategory' }

{ #category : '*Deprecated12' }
ToolShortcutsCategory >> openWorkspace [

	self deprecated: 'Use #openPlayground' transformWith: '`@rcv openWorkspace' -> '`@rcv openPlayground'.
	^ self openPlayground
]
