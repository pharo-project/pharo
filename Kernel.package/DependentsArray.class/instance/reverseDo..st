enumerating
reverseDo: aBlock
	"Refer to the comment in Collection|do:."
	| dep |
	self basicSize to: 1 by: -1 do: [:i |
		(dep := self basicAt: i) ifNotNil: [aBlock value: dep]]