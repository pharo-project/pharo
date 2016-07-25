panes
replacePane: oldPane with: newPane
	"Make newPane exactly occupy the position and extent of oldPane"

	| aLayoutFrame hadDep |
	hadDep := model dependents includes: oldPane.
	oldPane owner replaceSubmorph: oldPane by: newPane.
	newPane
		position: oldPane position;
		extent: oldPane extent.
	aLayoutFrame := oldPane layoutFrame.
	paneMorphs := paneMorphs collect:
		[:each |
		each == oldPane ifTrue: [newPane] ifFalse: [each]].
	aLayoutFrame ifNotNil: [newPane layoutFrame: aLayoutFrame].
	newPane color: Color transparent.
	hadDep ifTrue: [model removeDependent: oldPane. model addDependent: newPane].

	self changed

