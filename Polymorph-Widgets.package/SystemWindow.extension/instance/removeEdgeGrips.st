*Polymorph-Widgets
removeEdgeGrips
	"Remove the window edge grips."
	
	|edges|
	edges := self submorphsSatisfying: [:each | each isKindOf: WindowEdgeGripMorph].
	edges do: [:each | each delete]