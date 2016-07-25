menu morph
openMenuFor: aParagraphEditor 
	| theMenu |
	self stopCompletionDelay.
	
	context := self contextClass
				controller: self
				class: model selectedClassOrMetaClass 
				source: aParagraphEditor text string
				position: aParagraphEditor caret - 1.
	
	editor := aParagraphEditor.

	theMenu := self menuMorphClass
				controller: self
				position: (aParagraphEditor selectionPosition: context completionToken).
				
	theMenu isClosed
		ifFalse: [ menuMorph := theMenu ].
	
	(context isNil or: [ context model isEmpty ])
		ifTrue: [ ^self closeMenu ].