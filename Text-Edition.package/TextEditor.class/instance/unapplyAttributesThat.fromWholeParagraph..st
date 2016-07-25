private
unapplyAttributesThat: removalBlock fromWholeParagraph: wholeParagraph
	"The user selected aTextAttribute to be removed.
	If there is a selection, unapply the attribute to the selection.
	In any case do not use the attribute for the user input (emphasisHere)
	It really looks like the 2 branches in this method should be closer! (handling of Undo is different, and likely wrong)"

	| interval newText secondPass |

	self editingState emphasisHere: (self editingState emphasisHere reject: removalBlock).
	wholeParagraph
		ifTrue: [
			interval := self paragraph text encompassParagraph: self selectionInterval.
			newText := self paragraph text copyFrom: interval first to: interval last.
			newText
				removeAttributesThat: removalBlock
				replaceAttributesThat: [ :attr | false ]
				by: [ :attr | false ]
				signalIfWholeParagraphAttribute: false.
			self paragraph 
				replaceFrom: interval first 
				to: interval last 
				with: newText.
			self paragraph composeAll.
			self recomputeSelection ]
		
		ifFalse: [
			newText := self selection.
			newText notEmpty
				ifTrue: [
					"Do a first pass that will remove only regular attributes (i.e. isParagraphAttribute not)"
					secondPass := false.
					[
						newText
							removeAttributesThat: removalBlock
							replaceAttributesThat: [ :attr | false ]
							by: [ :attr | false ]
							signalIfWholeParagraphAttribute: true
					] on: Abort do: [ :ex |
						secondPass := true ].
					self replaceSelectionWith: newText.
					self paragraph composeAll.
					self recomputeSelection.
					"Do a second pass, calling this method again, that will remove self paragraph attributes from whole paragraphs"
					secondPass ifTrue: [
						self unapplyAttributesThat: [ :attr | attr isParagraphAttribute and: [removalBlock value: attr ]] fromWholeParagraph: true ] ] ].
	