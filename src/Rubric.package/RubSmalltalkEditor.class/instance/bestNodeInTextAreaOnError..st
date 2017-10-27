menu messages
bestNodeInTextAreaOnError: aBlock
	"Find the best node in the editor text area at the current pointer location"
	
	| node start stop |
	
	start := self textArea startIndex.
	stop := self textArea stopIndex.
	
	node := RBParser parseMethod: self textArea string onError: [ 
		RBParser parseExpression: self textArea string  onError: aBlock].
	
	^node bestNodeFor: (start to: stop).
	