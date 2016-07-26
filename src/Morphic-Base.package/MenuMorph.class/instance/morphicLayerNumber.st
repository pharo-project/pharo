private
morphicLayerNumber

	"helpful for insuring some morphs always appear in front of or behind others.
	smaller numbers are in front"
	^self valueOfProperty: #morphicLayerNumber  ifAbsent: [
		stayUp ifTrue:[100] ifFalse:[10]
	]