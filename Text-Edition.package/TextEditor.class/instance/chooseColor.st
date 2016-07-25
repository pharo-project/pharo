editing keys
chooseColor
	"Make a new Text Color Attribute, let the user pick a color, and return the attribute"

	| attribute |	
	attribute := TextColor color: Color black "default".
	UIManager default chooseColor
		ifNotNil: [:nc | attribute color: nc].
	^ attribute
