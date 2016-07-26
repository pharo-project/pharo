examples
autoReactingText
	"self autoReactingText"

	| area seg blk |
	area := RubEditingArea new
		beWrapped;
		withoutSelectionBar.
	area setTextWith: 'Click or keep the mouse down on [0]'.
	area width: 400.
	seg := RubPlainSegmentMorph inTextArea: area from: 34 to: 35.
	blk := [ seg replaceTextWith: (seg getText string asNumber + 1) asString ].
	seg on: #mouseDown send: #value to: blk.
	seg on: #mouseStillDown send: #value to: blk.
	seg beExtendable.
	area addSegment: seg.
	area openInWorld