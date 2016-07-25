enumerating
select: aBlock 
	"Refer to the comment in Collection|select:."
	| basicSize newSelf size selection |
	basicSize := self basicSize.
	newSelf := self species new: basicSize.
	size := 0.
      1 to: basicSize do: [ :i |
               (self basicAt: i) ifNotNil: [ :dep |
                       (aBlock value: dep) ifTrue: [
                               newSelf basicAt: (size := size+1) put: dep ] ] ].
	selection := self species new: size.
	selection basicReplaceFrom: 1 to: size with: newSelf startingAt: 1.
	^selection