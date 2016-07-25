Instances of this class accept, via #baseMorph:, a morph that is expected to be row-like containing submorphs.
Based on the receiver's layout, the base morphs that are able to fit within the receiver (subject to minExtents) are layed out along with, if necessary, a button to pop-up a column of any remaining, unfittable, base morphs.
Handy for button bars etc.

Example:

(OverflowRowMorph new
	baseMorph: (UITheme builder newRow: ((1 to: 6) collect: [:i | |label|
		label := 'Button ', i asString.
		(UITheme builder
			newButtonFor: Transcript
			getState: nil
			action: #show:
			arguments: {label}
			getEnabled: nil
			label: label
			help: nil)
			hResizing: #spaceFill]))) openInWindow
		
	