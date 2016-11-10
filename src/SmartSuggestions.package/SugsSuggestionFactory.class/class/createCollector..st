private
createCollector: key
	| collector |
	collector := PragmaCollector
		filter: [ :pragma | pragma selector = key ].
	collector reset.
	collector whenChangedSend: #reset to: collector.
	^ collector