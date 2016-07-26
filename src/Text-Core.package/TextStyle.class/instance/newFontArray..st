private
newFontArray: anArray 
	"Currently there is no supporting protocol for changing these arrays. If an editor wishes to implement margin setting, then a copy of the default should be stored with these instance variables.  
	, Make size depend on first font."
	fontArray := anArray.
	lineGrid := (fontArray at: 1) height + leading.	"For whole family"
	baseline := (fontArray at: 1) ascent + leading.
	alignment := 0.
	firstIndent := 0.
	restIndent := 0.
	rightIndent := 0.
	tabsArray := DefaultTabsArray.
	marginTabsArray := DefaultMarginTabsArray
	"
TextStyle allInstancesDo: [:ts | ts newFontArray: TextStyle default fontArray].
"