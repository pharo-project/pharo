helpers
initializeRendererWithIntervalFrom: startTextPosition to: endTextPosition
	
	interval := startTextPosition to: endTextPosition.
	renderer := TxHighlightRenderer highlighting: interval in: view with: nil.