scrolling
styler
	"The styler responsible for highlighting text in the receiver"
	^ styler ifNil: [styler := self defaultStyler]