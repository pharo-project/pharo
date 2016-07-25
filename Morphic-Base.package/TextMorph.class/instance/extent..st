geometry
extent: aPoint 
	| newExtent priorEditor |
	bounds extent = aPoint ifTrue: [^ self].
	priorEditor := editor.
	self isAutoFit
		ifTrue: [wrapFlag ifFalse: [^ self].  "full autofit can't change"
				newExtent := aPoint truncated max: self minimumExtent.
				newExtent x = self extent x ifTrue: [^ self].  "No change of wrap width"
				self releaseParagraphReally.  "invalidate the paragraph cache"
				super extent: newExtent.
				priorEditor
					ifNil: [self fit]  "since the width has changed..."
					ifNotNil: [self installEditorToReplace: priorEditor]]
		ifFalse: [super extent: (aPoint truncated max: self minimumExtent).
				wrapFlag ifFalse: [^ self].  "no effect on composition"
				self composeToBounds]
