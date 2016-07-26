dropping/grabbing
separateDragAndDrop
	"Conversion only. Separate the old #dragNDropEnabled into #dragEnabled and #dropEnabled and remove the old property."
	| dnd |
	(self hasProperty: #dragNDropEnabled) ifFalse:[^self].
	dnd := (self valueOfProperty: #dragNDropEnabled) == true.
	self dragEnabled: dnd.
	self dropEnabled: dnd.
	self removeProperty: #dragNDropEnabled.
