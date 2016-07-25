updating
update: aSymbol
	aSymbol ifNil: [ ^ self ].
	aSymbol == #flash
		ifTrue: [ ^ self flash ].
	aSymbol == self getTextSelector
		ifTrue: [ self updateTextWith: self getTextFromModel.
			^ self setSelection: self getSelectionFromModel ].
	aSymbol == self getSelectionSelector
		ifTrue: [ ^ self setSelection: self getSelectionFromModel ].
	aSymbol == #clearUserEdits
		ifTrue: [ ^ self hasUnacceptedEdits: false ].
	aSymbol == #wantToChange
		ifTrue: [ self canDiscardEdits
				ifFalse: [ ^ self promptForCancel ].
			^ self ].
	aSymbol == #appendEntry
		ifTrue:
			[ "Hack to manage the Transcript (Gloups) "
			self textArea
				handleEdit:
					[ "Knock off first half of text"
					self text size > self model characterLimit
						ifTrue: [ self textArea selectInvisiblyFrom: 1 to: self text size // 2.
							self textArea replaceSelectionWith: Text new ].	
					self appendText: self model contents asText ].
			^ self ].
	aSymbol == #clearText
		ifTrue: [ self textArea handleEdit: [ self updateTextWith: Text new ].
			^ self ].
	aSymbol == #bs
		ifTrue: [ self handleEdit: [ self updateTextWith: (self text copyFrom: 1 to: (self text size - 1 max: 0)) ].
			^ self ].
	aSymbol == #codeChangedElsewhere
		ifTrue: [ self hasEditingConflicts: true.
			^ self ].
	aSymbol == self getEnabledSelector
		ifTrue: [ self updateEnabled ].
	^ self