container protocol
rectanglesAt: lineY height: lineHeight 
	"Return a list of rectangles that are at least minWidth wide
	in the specified horizontal strip of the shadowForm.
	Cache the results for later retrieval if the owner does not change."

	| hProfile rects thisWidth thisX count pair outerWidth lineRect lineForm |
	pair := Array with: lineY with: lineHeight.
	rects := rectangleCache at: pair ifAbsent: [nil].
	rects ifNotNil: [^rects].
	outerWidth := minWidth + (2 * OuterMargin).
	self shadowForm.	"Compute the shape"
	lineRect := 0 @ (lineY - shadowForm offset y) 
				extent: shadowForm width @ lineHeight.
	lineForm := shadowForm copy: lineRect.

	"Check for a full line -- frequent case"
	(lineForm tallyPixelValues second) = lineRect area 
		ifTrue: 
			[rects := Array with: (shadowForm offset x @ lineY extent: lineRect extent)]
		ifFalse: 
			["No such luck -- scan the horizontal profile for segments of minWidth"

			hProfile := lineForm xTallyPixelValue: 1 orNot: false.
			rects := OrderedCollection new.
			thisWidth := 0.
			thisX := 0.
			1 to: hProfile size
				do: 
					[:i | 
					count := hProfile at: i.
					count >= lineHeight 
						ifTrue: [thisWidth := thisWidth + 1]
						ifFalse: 
							[thisWidth >= outerWidth 
								ifTrue: 
									[rects addLast: ((thisX + shadowForm offset x) @ lineY 
												extent: thisWidth @ lineHeight)].
							thisWidth := 0.
							thisX := i]].
			thisWidth >= outerWidth 
				ifTrue: 
					[rects addLast: ((thisX + shadowForm offset x) @ lineY 
								extent: thisWidth @ lineHeight)]].
	rects := rects collect: [:r | r insetBy: OuterMargin @ 0].
	rectangleCache at: pair put: rects.
	^rects