scripting
andShow: aBlock
	"This method accepts a block with one argument.
	The block will be evaluated with an instance of GLMCompositePresentation.
	
	The block should construct the presentations that go in the composite presentation.
	The composite presentation will be the one that will get installed in the target pane
	once the transmission triggers.

	This method is meant to be used in a Glamour script.
	"
	
	self ensureReplacePresentationsStrategy.
	self transmissionStrategy presentationsFactory: aBlock.
	self origins isEmpty
		ifTrue: [ self from: #outer port: #entity ]