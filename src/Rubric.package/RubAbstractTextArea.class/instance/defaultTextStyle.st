defaults
defaultTextStyle
	| ts f |
	f := self class editorFont.
	ts := f textStyle copy ifNil: [ TextStyle fontArray: { f } ].
	ts defaultFontIndex: (ts fontIndexOf: f).
	^ ts