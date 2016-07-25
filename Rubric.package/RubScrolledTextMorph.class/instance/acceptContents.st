model protocol
acceptContents
	(self autoAccept not and: [ self canDiscardEdits and: [ self alwaysAccept not ] ])
		ifTrue: [ ^ self flash ].
	self hasEditingConflicts
		ifTrue: [  
			(self
				confirm:
					'Caution! This method may have been
changed elsewhere since you started
editing it here.  Accept anyway?' translated)
				ifFalse: [ ^ self flash ] ].
	self acceptTextInModel == true
		ifTrue: [
			self hasUnacceptedEdits: false.
			self rulers do: [ :r | r textAccepted ].
			self announcer announce: (RubTextAccepted morph: self) ]