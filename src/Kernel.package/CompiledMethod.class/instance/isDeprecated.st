testing
isDeprecated
	"Object selectorsInProtocol: #deprecation"
	(self
		sendsAnySelectorOf:
			#(#deprecated: #deprecated:on:in: #deprecated:on:in:transformWith: #deprecated:transformWith:))
		ifTrue: [ ^ true ].
	$-
		split: self protocol asString
		do: [ :each | 
			each withBlanksCondensed = 'deprecated'
				ifTrue: [ ^ true ] ].
	^ false