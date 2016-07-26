events
mouseUp: evt
	"An attempt to break up the old processRedButton 
	code into three phases"
	| mouseDownInterval clickPoint b|
	
	(mouseDownInterval := self editingState mouseDownInterval)
		ifNil: [^ self]. 
		
	(self paragraph click: evt for: model controller: self ) 
		ifTrue: [
			self updateSelectionFromEvent: evt.
			evt hand releaseKeyboardFocus: self.
			^ self ].

	(evt yellowButtonChanged or: [ evt commandKeyPressed]) 
		ifTrue: [
			(self morph editView yellowButtonActivity: evt shiftPressed) 
				ifTrue: [ ^ self ]].
		
	(self hasCaret and: [mouseDownInterval = self selectionInterval])
		ifTrue: [self selectWord].
		
	self setEmphasisHereFromText.
	self storeSelectionInParagraph