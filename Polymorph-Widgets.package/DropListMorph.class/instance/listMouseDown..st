event handling
listMouseDown: evt
	"Click outside the list."

	(self listMorph fullContainsPoint: evt position)
		ifTrue: [ 
			self listMorph changeModelSelection: (self listMorph rowAtLocation: evt position) ].
	self hideList