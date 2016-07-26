menu messages
changeStyle
	"Let user change styles for the current text pane."

	| names reply style current menuList |
	current := TextStyle default copy.
	names := TextStyle actualTextStyles keys.
	menuList := names collect: [ :styleName |
		(styleName = current name) -> styleName ].
	reply := UIManager default chooseFrom:  menuList values: names.	
	reply ifNotNil: [
		(style := TextStyle named: reply) ifNil: [ ^ true].
		self text style: style.
		self paragraph textStyle: style.
		self paragraph composeAll.
		self recomputeSelection].
	^ true