private
handleEdit: editBlock
	"We override this hook method to intercept pasting in the text morph.
	When pasting happens, we update the text port"
	super handleEdit: editBlock.
	self announcer announce: (GLMTextChanged new text: self text)