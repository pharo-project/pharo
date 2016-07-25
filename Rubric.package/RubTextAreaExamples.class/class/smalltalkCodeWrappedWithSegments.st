examples
smalltalkCodeWrappedWithSegments
	"self smalltalkCodeWrappedWithSegments"

	| area s |
	area := self smalltalkCodeWrappedTo400.
	area addSegment: (RubUnderlinedSegmentMorph inTextArea: area from: 1 to: 29).
	s := RubUnderlinedSegmentMorph inTextArea: area from: 43 to: 57.
	s straight: false.
	area addSegment: s.
	area addSegment: (s := RubUnderlinedSegmentMorph inTextArea: area from: 81 to: 185).
	s color: (Color blue alpha: 0.02).
	s borderColor: Color blue.
	s underlineColor: Color blue.
	area addSegment: (s := RubPlainSegmentMorph inTextArea: area from: 300 to: 500).
	s color: (Color yellow).
	s borderColor: (Color orange alpha: 0.5).
	s beSmoothCurve.
	s balloonColor: Color yellow.
	s setCenteredBalloonText: 'Please note this deferred execution'.
	area openInWorld