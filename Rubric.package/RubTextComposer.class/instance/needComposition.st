private composition
needComposition
	| ret |
	ret := prevText isNil.
	ret ifFalse: [ret := prevTextStyle ~= theTextStyle].
	ret ifFalse: [ret := prevContainer ~= theContainer].
	ret ifFalse: [ret := prevText string ~= theText string].
	ret ifFalse: [ret := prevText runs ~= theText runs].
	ret ifTrue: [
		prevText := theText copy.
		prevTextStyle := theTextStyle.
		prevContainer := theContainer].
	^ ret