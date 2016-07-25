morphic
newContentMorph
	^ (self newRow: {
		self newLabelGroup: {
			'Selector:' -> (selectorField:=(self newTextEntryFor: self getText: #selector setText: #selector: help: nil)
				autoAccept: true;
				on: #keyStroke send: #value:value: to: [:key :morph | key keyCharacter = Character cr ifTrue: [ self ok. true]. false];
				yourself).
			'Arguments:' -> (self newRow: {
				(self newListFor: self list: #argumentList selected: #argumentIndex changeSelected: #argumentIndex: help: nil)
					hResizing: #spaceFill;
					yourself.
				(self newColumn: {
					(self newButtonFor: self action: #up label: 'up'  help: nil)
						getEnabledSelector: #isUpEnabled.
					(self newButtonFor: self action: #down label: 'dn'  help: nil)
						getEnabledSelector: #isDownEnabled })
					hResizing: #shrinkWrap }).
			'Preview:' -> (labelMorph := self newLabel: self methodName printString) } })
 		minWidth: 400;
		yourself