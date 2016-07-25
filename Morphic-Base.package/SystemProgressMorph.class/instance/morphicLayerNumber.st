initialization
morphicLayerNumber
	"progress morphs are behind menus and balloons, but in front of most other stuff"
	^self valueOfProperty: #morphicLayerNumber ifAbsent: [ 12 ].
