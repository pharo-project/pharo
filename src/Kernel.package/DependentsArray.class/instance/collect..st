enumerating
collect: aBlock 
	"Refer to the comment in Collection|select:."
	| basicSize newSelf size dep selection |
	basicSize := self basicSize.
	newSelf := self species new: basicSize.
	size := 0.
	1 to: basicSize do: [:i |
		(dep := self basicAt: i) notNil ifTrue: [newSelf basicAt: (size := size+1) put: (aBlock value: dep)]].
	selection := self species new: size.
	selection basicReplaceFrom: 1 to: size with: newSelf startingAt: 1.
	^selection