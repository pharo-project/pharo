layout
layoutPanes
	| widths rect |
	widths := self computeMorphWidths.
	rect := 0@0 extent: (0 @ self paneHeight).
	transform submorphs 
					with: widths 
					do: [:m :w | 
						rect := rect withWidth: w.
						m bounds: rect.
						rect := rect translateBy: (w@0)]
						
