"
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

"
Class {
	#name : #SHWorkspaceEditingMode,
	#superclass : #Model,
	#instVars : [
		'code'
	],
	#category : #'Shout-Examples'
}

{ #category : #'private bindings' }
SHWorkspaceEditingMode >> bindings [
	^ #('shout' 'example')
]

{ #category : #accessing }
SHWorkspaceEditingMode >> code [
	^ code ifNil: [code := '' asText]
]

{ #category : #accessing }
SHWorkspaceEditingMode >> code: someText [
	code := someText.
	self changed: #code
]

{ #category : #bindings }
SHWorkspaceEditingMode >> hasBindingOf: aString [
	^ self bindings includes: aString asSymbol.
]

{ #category : #bindings }
SHWorkspaceEditingMode >> hasBindingThatBeginsWith: aString [
	^ self bindings anySatisfy: [:s | s beginsWith: aString]

]

{ #category : #view }
SHWorkspaceEditingMode >> open [
	| window editor |
	window := (StandardWindow labelled: 'Editor with Workspace like  syntax highlighting') model: self.
	editor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	"Set-up the styler to be in workspace mode"
	editor styler workspace: self.
	window addMorph: editor frame: (0@0 extent: 1@1).
	^ window openInWorld
]

{ #category : #shout }
SHWorkspaceEditingMode >> shoutAboutToStyle: aTextView [
	^ true
]
