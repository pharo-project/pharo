private
glamorousThickBorderFor: aWindow
	| aStyle desiredColor |
	"Color lightGray is the color with which SystemWindow initializes the border color.
	If it is the default, we override it with the theme specific color.
	Otherwise, we choose the desired color"
	desiredColor := 
		(aWindow borderColor isNil or: [
			aWindow borderColor isColor not or: [ "Prevent crashes for #raised or #inset borders"
				aWindow borderColor = Color lightGray]])
							ifTrue: [ self glamorousDarkBaseColorFor: aWindow ]
							ifFalse: [ aWindow borderColor ].
	aStyle := SimpleBorder new
					baseColor: desiredColor;
					width: 2.
	aWindow borderStyle: aStyle.
	^ aStyle