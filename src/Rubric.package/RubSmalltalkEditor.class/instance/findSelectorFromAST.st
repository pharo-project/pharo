menu messages
findSelectorFromAST
	"Try to make a selector out of the current text selection"

	| node fallbackBlock |

	fallbackBlock := [^ ''].
	node := self bestNodeInTextAreaOnError: fallbackBlock.
	node isSequence ifTrue: [
		self selectLine.
		node := self bestNodeInTextAreaOnError: fallbackBlock]. 
	node isMethod ifFalse: [ 
		[ node isMessage ] whileFalse: [ 
	 		(node := node parent) ifNil: fallbackBlock ]].
	
	^node selector