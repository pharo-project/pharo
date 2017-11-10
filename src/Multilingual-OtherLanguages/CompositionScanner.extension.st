Extension { #name : #CompositionScanner }

{ #category : #'*Multilingual-OtherLanguages' }
CompositionScanner >> registerBreakableIndex [
	"Record left x and character index of the line-wrappable point. 
	Used for wrap-around in eastern Asian languages."

	spaceX := destX.
	lineHeightAtSpace := lineHeight.
	baselineAtSpace := baseline.
	spaceIndex := lastIndex.
	lastBreakIsNotASpace := true.
]
