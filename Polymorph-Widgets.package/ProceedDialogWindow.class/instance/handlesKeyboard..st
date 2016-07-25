as yet unclassified
handlesKeyboard: evt
	"Return true if the receiver wishes to handle the given keyboard event"
	
	(super handlesKeyboard: evt) ifTrue: [^true].
	^evt keyCharacter = $y or: [
		evt keyCharacter = $n]
	