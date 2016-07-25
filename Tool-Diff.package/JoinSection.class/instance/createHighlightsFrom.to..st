as yet unclassified
createHighlightsFrom: srcPara to: dstPara
	"Create and store the src and dst highlights.
	Use the given paragraphs to determine inline
	diffs."

	| s d si di srcText dstText diffs i sb eb line |
	self createHighlights.
	self src lineRange notEmpty
		ifTrue: [ 
			line := srcPara lines at: self src lineRange first.
			si := line first.
			line := srcPara lines at: self src lineRange last.
			srcText := srcPara string copyFrom: si to: line last ]
		ifFalse: [ srcText := '' ].
	self dst lineRange notEmpty
		ifTrue: [ 
			line := dstPara lines at: self dst lineRange first.
			di := line first.
			line := dstPara lines at: self dst lineRange last.
			dstText := dstPara string copyFrom: di to: line last ]
		ifFalse: [ dstText := '' ].
	self src text: srcText.
	self dst text: dstText.
	self type = #modification
		ifFalse: [ ^ self ].
	s := self src highlights.
	d := self dst highlights.
	diffs := (InlineTextDiffBuilder from: srcText to: dstText) buildPatchSequence aggregateRuns: [ :e | e key ].
	diffs
		do: [ :c | 
			c first key = #match
				ifTrue: [ 
					c
						do: [ :a | 
							si := si + a value size.
							di := di + a value size ] ].
			c first key = #insert
				ifTrue: [ 
					i := di.
					c do: [ :a | di := di + a value size ].
					sb := dstPara characterBlockForIndex: i.
					eb := dstPara characterBlockForIndex: di - 1.
					self
						addHighlightsFrom: sb
						to: eb
						to: d
						color: (Color green alpha: 0.3) ].
			c first key = #remove
				ifTrue: [ 
					i := si.
					c do: [ :a | si := si + a value size ].
					sb := srcPara characterBlockForIndex: i.
					eb := srcPara characterBlockForIndex: si - 1.
					self
						addHighlightsFrom: sb
						to: eb
						to: s
						color: (Color red alpha: 0.3) ] ]