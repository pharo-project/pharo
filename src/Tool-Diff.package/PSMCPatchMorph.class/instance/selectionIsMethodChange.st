private
selectionIsMethodChange
	"Answer whether the currently selected change is for a method."

	^self selectedChange notNil and: [ 
		self selectedChange definition isMethodDefinition and: [ 
			self selectedChange definition isLoadable ] ]