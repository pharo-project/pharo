event handling
handlesKeyboard: evt
	"Return true if the receiver wishes to handle the given keyboard event"
	
	(super handlesKeyboard: evt) ifTrue: [^true].
	^evt keyCharacter = Character escape or: [
		(self defaultButton notNil and: [
			evt keyCharacter = Character cr])]
	