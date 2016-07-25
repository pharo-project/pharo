private
installEditorToReplace: priorEditor
	"Install an editor for my paragraph.  This constitutes 'hasFocus'.
	If priorEditor is not nil, then initialize the new editor from its state.
	We may want to rework this so it actually uses the prior editor."

	| stateArray |
	priorEditor ifNotNil: [stateArray := priorEditor stateArray].
	editor := self editorClass forMorph: self.
	priorEditor ifNotNil: [editor stateArrayPut: stateArray].
	self selectionChanged.
	^ editor