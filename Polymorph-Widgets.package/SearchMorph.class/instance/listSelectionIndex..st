accessing
listSelectionIndex: anInteger
	"Set the list selection."

	self hideList.
	anInteger = 0 
		ifTrue: [
			self model ifNotNil: [:m | 
				self setIndexSelector ifNotNil: [:s |
					self useSelectionIndex 
						ifFalse: [m perform: s with: nil]]]]
		ifFalse: [ ^ super listSelectionIndex: anInteger ].