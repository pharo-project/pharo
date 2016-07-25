ui-building
newChoicesMorph
	"Answer a row of columns of buttons and separators based on the model."

	| answer morphs str  |
	answer := self newRow
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	self labels ifEmpty: [^answer].
	self labels first isNil
		ifTrue: [self labels: self labels allButFirst].
	self labels ifEmpty: [^answer].
	self labels last isNil
		ifTrue: [self labels: self labels allButLast].
	self labels ifEmpty: [^answer].
	morphs := OrderedCollection new.
	1 to: self labels size do: [:i |
		morphs add: (self newChoiceButtonFor: i)].
	str := morphs readStream.
	[str atEnd] whileFalse: [
		answer
			addMorphBack: (self newMenuWith: (str next: self maxLines));
			addMorphBack: self newVerticalSeparator].
	answer removeMorph: answer submorphs last.
	answer submorphs last hResizing: #spaceFill.
	scrollPaneWidth ifNotNil: [answer hResizing: #spaceFill. answer submorphs last hResizing: #rigid. answer submorphs last width: scrollPaneWidth].
	self choiceMenus: (answer submorphs select: [:m| m isMenuMorph]).
	^answer