private
createCollector: key 
	| collector |
	collector :=  PragmaCollector filter: [:pragma | pragma keyword=key].
	collector reset.
	collector whenChangedSend: #reset to: collector.
	^collector.
