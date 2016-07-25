meta-actions
showActions
	"Put up a message list browser of all the code that this morph  
	would run for mouseUp, mouseDown, mouseMove, mouseEnter,  
	mouseLeave, and mouseLinger."
	
	| list cls selector adder |
	list := SortedCollection new.
	adder := [:mrClass :mrSel | list
				add: (RGMethodDefinition realClass: mrClass selector: mrSel)].
	"the eventHandler"
	self eventHandler
		ifNotNil: [list := self eventHandler methodRefList.
			(self eventHandler handlesMouseDown: nil)
				ifFalse: [adder value: HandMorph value: #grabMorph:]].
	"If not those, then non-default raw events"
	#(#keyStroke: #mouseDown: #mouseEnter: #mouseLeave: #mouseMove: #mouseUp: #doButtonAction )
		do: [:sel | 
			cls := self class whichClassIncludesSelector: sel.
			cls
				ifNotNil: ["want more than default behavior"
					cls == Morph
						ifFalse: [adder value: cls value: sel]]].
	"The mechanism on a Button"
	(self respondsTo: #actionSelector)
		ifTrue: ["A button"
			selector := self actionSelector.
			cls := self target class whichClassIncludesSelector: selector.
			cls
				ifNotNil: ["want more than default behavior"
					cls == Morph
						ifFalse: [adder value: cls value: selector]]].
	Smalltalk tools messageList browse: list title:  'Actions of ' , self printString.