"
I am a command decorator adding informations useful when for usage in context of a Spec application.

Basically, I add:
- an #icon (#blank by default)
- a #shortcutKey (optional)
- a #displayStrategy to handle how to show the command if it is not executable on a context (default is to be disabled)
- a #positionStrategy to handle on which side presenter with left and right side I should be (default is left)

"
Class {
	#name : #SpCommand,
	#superclass : #CmUICommand,
	#category : #'Spec2-Commander2-Core'
}

{ #category : #converting }
SpCommand >> asButtonPresenter [
	self flag: #TODO. "Needs to use inform user display strategy when available, no other available strategy can be used in this context. See issue #705"
	^ SpButtonPresenter new
		label: self name;
		help: self description;
		in: [ :button |
			self hasIcon
				ifTrue: [ button iconName: self iconName ] ];
		action: [ self execute ]
		yourself
		
]

{ #category : #displaying }
SpCommand >> displayIn: aMenuGroupOrPresenter do: aBlock [

	self displayStrategy 
		display: self 
		in: aMenuGroupOrPresenter 
		do: aBlock
]
