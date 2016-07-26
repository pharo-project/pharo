private
computeInsetColors
	| base light dark w colorArray param hw |
	base := self color asColor.
	light := Color white.
	dark := Color black.
	w := self width isPoint 
				ifTrue: [self width x max: self width y]
				ifFalse: [self width].
	w := w asInteger.
	colorArray := Array new: w * 2.
	hw := 0.5 / w.
	0 to: w - 1
		do: 
			[:i | 
			param := true 
				ifTrue: [ 0.5 + (hw * i)]
				ifFalse: [0.5 + (hw * (w - i))].
			colorArray at: i + 1 put: (base mixed: param with: dark).	"darken"
			colorArray at: colorArray size - i put: (base mixed: param with: light)	"brighten"].
	^colorArray