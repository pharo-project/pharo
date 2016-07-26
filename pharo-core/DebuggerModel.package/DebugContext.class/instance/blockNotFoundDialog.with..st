private
blockNotFoundDialog: aMethod with: aText

	| browser message result |
	
	message := 'Method for block not found on stack, can''t edit and continue'.
	
	"shouldn't edit doits"
	aMethod selector isDoIt
		ifTrue: [ 	^ self inform: message ].
			
	result := UIManager default
						confirm: message
						trueChoice: 'Browse' translated
						falseChoice: 'Cancel' translated.
						
	"possible return values are true | false | nil"
	result == true ifFalse: [ ^ self ].
		
	"let's browse the given method with the edited contents"
	browser := aMethod browse.
	browser 
		contents: aText; 
		changed: #contents.
	browser codeTextMorph 
		hasUnacceptedEdits: true