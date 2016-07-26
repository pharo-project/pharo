as yet unclassified
selectNextConflict
	"Select the next conflict after the current selection, if any."

	|next op def path|
	next := self nextConflict ifNil: [^self].
	op := next operation.
	def := next remoteDefinition ifNil: [next localDefinition].
	path := {#changes. #openPath}.
	def isMethodDefinition ifTrue: [path := path, {def fullClassName}].
	path := path, {op shortSummary}.
	self changed: path