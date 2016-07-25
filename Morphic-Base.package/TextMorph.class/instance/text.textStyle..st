private
text: t textStyle: s
	"Private -- for use only in morphic duplication"
	text := t.
	defaultColor := (t attributesAt: 1) detect: [ :e | e isKindOf: TextColor ] ifNone: [ self color ].
	textStyle := s.
	paragraph ifNotNil: [paragraph textStyle: s]