visiting
visitMessageNode: msgNode
	"Do not add return statements. 
	ifTrue:ifFalse: handling may change the current message node into an ifTrue: , an ifFalse: or an ifNil:ifNotNil: message. 
	whileTrue are used to detect to:do: and to: by: do."

	super visitMessageNode: msgNode.
	(self isReconstructed: msgNode)
		ifFalse: [ ^ self ].
	msgNode selector = #ifTrue:ifFalse:
		ifTrue: [ self handleIfTrueIfFalse: msgNode ].
	msgNode selector = #ifNil:ifNotNil:
		ifTrue: [ self handleIfNilIfNotNil: msgNode ].
	msgNode selector = #ifTrue:
		ifTrue: [ self handleIfTrue: msgNode ].
	msgNode selector = #ifFalse:
		ifTrue: [ self handleIfFalse: msgNode ].
	msgNode selector = #whileTrue:
		ifTrue: [ self handleWhileTrue: msgNode ].
	(self isBigArray: msgNode)
		ifTrue: [ self handleBigArray: msgNode ]