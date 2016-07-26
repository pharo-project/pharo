By default, shout styling is into a method editing mode. If you need a script editor for example, then you have to setup shout into a workspace mode. In order to setup a workspace mode, one have to make it available the workspace instance to the view styler. Typically, the workspace inspace is the model instance itself. See the sent of the #workspace: message into the #open method:

open
	...
	editor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	editor styler workspace: self. 
	...
	^ window openInWorld

The workspace class has also to implement #hasBindingOf: and #hasBindingThatBeginsWith:. In fact, #hasBindingOf: and #hasBindingThatBeginsWith: are sent to the workspace by shout during styling. As an example, see the #bindings method implemented here:

bindings
	^ #('shout' 'styling')

It returns a list of symbols which is used by #hasBindingOf: and #hasBindingThatBeginsWith:. 
Then open an editor with:

SHWorkspaceEditingMode new open

If you enter the word 'shout' as an example, you will see that the word is recognized as a variable name.
