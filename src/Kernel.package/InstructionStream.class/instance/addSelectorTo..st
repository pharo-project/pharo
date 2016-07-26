scanning
addSelectorTo: set 
	"If this instruction is a send, add its selector to set."

	| selectorOrSelf |
	(selectorOrSelf := self selectorToSendOrSelf) == self ifFalse:
		[set add: selectorOrSelf]