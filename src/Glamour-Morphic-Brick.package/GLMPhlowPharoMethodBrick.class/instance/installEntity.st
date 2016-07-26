phlow
installEntity
	self textBrick model: self.
	self textBrick classOrMetaClass: (self smalltalkClassLogic phlowValue: self entity).
	self textBrick setTextWith: (self displayLogic phlowValue: self entity).
	self textBrick selectionInterval: (self selectionInterval phlowValue: self entity)