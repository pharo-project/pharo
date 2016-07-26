geometry
minimumExtent
	| ext |
	"This returns the minimum extent that the morph may be shrunk to.  Not honored in too many places yet, but respected by the resizeToFit feature, at least.  copied up from SystemWindow 6/00"
	(ext := self valueOfProperty: #minimumExtent)
		ifNotNil:
			[^ ext].
	^ 100 @ 80