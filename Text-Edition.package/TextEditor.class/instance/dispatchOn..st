typing support
dispatchOn: aKeyboardEvent
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."
	"TODO rename to dispatch: "
	
	| char return keyEvents|
 	return := [:val| ^ val].

	self acceptCR: aKeyboardEvent return: return.
	self clearParens.
	self dispatchEnterOn: aKeyboardEvent return: return.
	self dispatchCommandOn: aKeyboardEvent return: return.
			
	char := aKeyboardEvent keyCharacter.
      ( ')]}'  includes: char) 
		ifTrue: [ self blinkPrevParen: char ].
		
       ^ self normalCharacter: aKeyboardEvent.
