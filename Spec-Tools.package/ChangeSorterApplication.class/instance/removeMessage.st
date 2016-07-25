menu - message
removeMessage
	"Remove the selected message from the system."
	
	self okToChange
		ifFalse: [^ self].
	self selectedSelector
		ifNotNil: [:selector | | class method | 
			class := self selectedClass.
				(class includesSelector: selector) ifFalse:[^ self].
				method := class>>selector.
				(self model removeMethod: method inClass: class)
					ifTrue: [ self updateMessagesList ]]