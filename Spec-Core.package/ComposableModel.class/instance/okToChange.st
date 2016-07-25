private
okToChange

	^ owner 
		ifNil: [
			self canDiscardEdits ifTrue: [^ true].
			self changed: #wantToChange.  "Solicit cancel from view"
			^ self canDiscardEdits]
		ifNotNil: [ owner okToChange ]