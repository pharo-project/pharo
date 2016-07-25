display
displaySelectionInLine: line on: aCanvas 
	| leftX rightX w caretColor |
	selectionStart ifNil: [^self].	"No selection"
	aCanvas isShadowDrawing ifTrue: [ ^self ].	"don't draw selection with shadow"
	selectionStart = selectionStop 
		ifTrue: 
			["Only show caret on line where clicked"

			selectionStart textLine ~= line ifTrue: [^self]]
		ifFalse: 
			["Test entire selection before or after here"

			(selectionStop stringIndex < line first 
				or: [selectionStart stringIndex > (line last + 1)]) ifTrue: [^self].	"No selection on this line"
			(selectionStop stringIndex = line first 
				and: [selectionStop textLine ~= line]) ifTrue: [^self].	"Selection ends on line above"
			(selectionStart stringIndex = (line last + 1) 
				and: [selectionStop textLine ~= line]) ifTrue: [^self]].	"Selection begins on line below"
	leftX := (selectionStart stringIndex < line first 
				ifTrue: [line ]
				ifFalse: [selectionStart ])left.
	rightX := (selectionStop stringIndex > (line last + 1) or: 
					[selectionStop stringIndex = (line last + 1) 
						and: [selectionStop textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [selectionStop left].
	selectionStart = selectionStop 
		ifTrue: 
			[rightX := rightX + 1.
			w := self caretWidth.
			caretRect := (leftX-w) @ line top corner: (rightX+w)@ line bottom.
			self showCaret ifFalse:[^self].
			caretColor := self insertionPointColor.
			1 to: w
				do: 
					[:i | 
					"Draw caret triangles at top and bottom"

					aCanvas fillRectangle: ((leftX - w + i - 1) @ (line top + i - 1) 
								extent: ((w - i) * 2 + 3) @ 1)
						color: caretColor.
					aCanvas fillRectangle: ((leftX - w + i - 1) @ (line bottom - i) 
								extent: ((w - i) * 2 + 3) @ 1)
						color: caretColor].
			aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom)
				color: caretColor]
		ifFalse: 
			[caretRect := nil.
			aCanvas fillRectangle: (leftX @ line top corner: rightX @ line bottom)
				color: self selectionColor]