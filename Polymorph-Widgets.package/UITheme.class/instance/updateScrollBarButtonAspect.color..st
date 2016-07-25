scrollbars
updateScrollBarButtonAspect: aMorph color: aColor 
	"update aMorph with aColor"
	aMorph isNil ifTrue: [^ self].
	aMorph color: aColor.
