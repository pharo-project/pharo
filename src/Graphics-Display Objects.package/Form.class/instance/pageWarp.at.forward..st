transitions
pageWarp: otherImage at: topLeft forward: forward
	"Produce a page-turning illusion that gradually reveals otherImage
	located at topLeft in this form.
	forward == true means turn pages toward you, else away. [ignored for now]"
	| pageRect oldPage nSteps buffer p leafRect sourceQuad warp oldBottom d |
	pageRect := otherImage boundingBox.
	oldPage := self copy: (pageRect translateBy: topLeft).
	(forward ifTrue: [oldPage] ifFalse: [otherImage])
		border: pageRect
		widthRectangle: (Rectangle
				left: 0
				right: 2
				top: 1
				bottom: 1)
		rule: Form over
		fillColor: Color black.
	oldBottom := self copy: ((pageRect bottomLeft + topLeft) extent: (pageRect width@(pageRect height//4))).
	nSteps := 8.
	buffer := Form extent: otherImage extent + (0@(pageRect height//4)) depth: self depth.
	d := pageRect topLeft + (0@(pageRect height//4)) - pageRect topRight.
	1 to: nSteps-1 do:
		[:i | forward
			ifTrue: [buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
					p := pageRect topRight + (d * i // nSteps)]
			ifFalse: [buffer copy: pageRect from: oldPage to: 0@0 rule: Form over.
					p := pageRect topRight + (d * (nSteps-i) // nSteps)].
		buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
		leafRect := pageRect topLeft corner: p x @ (pageRect bottom + p y).
		sourceQuad := Array with: pageRect topLeft
			with: pageRect bottomLeft + (0@p y)
			with: pageRect bottomRight
			with: pageRect topRight - (0@p y).
		warp := (WarpBlt toForm: buffer)
				clipRect: leafRect;
				sourceForm: (forward ifTrue: [oldPage] ifFalse: [otherImage]);
				combinationRule: Form paint.
		warp copyQuad: sourceQuad toRect: leafRect.
		self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
		Display forceDisplayUpdate].

	buffer copy: pageRect from: otherImage to: 0@0 rule: Form over.
	buffer copy: oldBottom boundingBox from: oldBottom to: pageRect bottomLeft rule: Form over.
	self copy: buffer boundingBox from: buffer to: topLeft rule: Form over.
	Display forceDisplayUpdate.
"
1 to: 4 do: [:corner | Display pageWarp:
				(Form fromDisplay: (10@10 extent: 200@300)) reverse
			at: 10@10 forward: false]
"
