debugger access
stackOfSize: limit 
	"Answer an OrderedCollection of the top 'limit' contexts
	 on the receiver's sender chain."

	| stack context |
	stack := OrderedCollection new.
	stack addLast: (context := self).
	[(context := context sender) ~~ nil
	 and: [stack size < limit]] whileTrue: [
		stack addLast: context ].
	^ stack