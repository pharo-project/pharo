defaults
defaultGhostTextMorph
	| gt |
	gt := RubEditingArea new.
	gt withoutAnyDecorator.
	gt backgroundColor: Color transparent.
	gt beReadOnly.
	gt beWrapped.
	^ gt