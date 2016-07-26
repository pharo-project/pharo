as yet unclassified
newChoicesMorph
	"Answer a row of columns of buttons and separators based on the model."

	|answer morphs str maxLines|
	answer := self newRow
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	self labels ifEmpty: [^answer].
	maxLines := Display height - 100 // 2 // (self newChoiceButtonFor: 1) height.
	morphs := OrderedCollection new.
	1 to: self labels size do: [:i |
		morphs add: (self newChoiceButtonFor: i).
		(self lines includes: i) ifTrue: [
			morphs add: self newSeparator]].
	str := morphs readStream.
	[str atEnd] whileFalse: [
		answer
			addMorphBack: (self newMenuWith: (str next: maxLines));
			addMorphBack: self newVerticalSeparator].
	answer removeMorph: answer submorphs last.
	answer submorphs last
		hResizing: #spaceFill.
	self choiceMenus: (answer submorphs select: [:m| m isKindOf: MenuMorph]).
	^answer