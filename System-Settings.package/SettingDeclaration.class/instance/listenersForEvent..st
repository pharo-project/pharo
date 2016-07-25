user interface
listenersForEvent: anEvent
	| listeners actionSequence |	
	listeners := Set new.	
	actionSequence := self actionMap at: anEvent ifAbsent: [].	
	actionSequence ifNotNil: [
		actionSequence isCollection	 
			ifTrue: [actionSequence do: [:oneAction | 
					oneAction receiver ifNotNil: [:r | listeners add: r]]]
			ifFalse: [listeners add: actionSequence receiver]].
	^ listeners