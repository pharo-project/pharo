accessing-tags
tags
	"Any method could be tagged with multiple symbols for user purpose. 
	For now we only define API to manage them implemented on top of method protocols.
	Protocol unclassified means that method is not tagged by anything"
	| protocol |
	protocol := self protocol.
	protocol ifNil: [ ^#() ].
	protocol = Protocol unclassified ifTrue: [ ^#() ].
	^{protocol}