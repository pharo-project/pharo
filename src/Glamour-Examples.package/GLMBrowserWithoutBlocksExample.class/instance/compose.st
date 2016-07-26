as yet unclassified
compose
	| wrapper |
	wrapper := self wrapper.
	wrapper show: [ :a |
		a list 
			display: (MessageSend receiver: self selector: #displayList:) ]