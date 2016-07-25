editing keys
shiftEnclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."

	| char left right startIndex stopIndex oldSelection which text |
	char := aKeyboardEvent keyCharacter.
	char = $9 ifTrue: [ char := $( ].
	char = $, ifTrue: [ char := $< ].
	char asciiValue = 27 ifTrue: [ char := ${ ].	"ctrl-["

	self closeTypeIn.
	startIndex := self startIndex.
	stopIndex := self stopIndex.
	oldSelection := self selection.
	which := '([<{"''' indexOf: char ifAbsent: [1].
	left := '([<{"''' at: which.
	right := ')]>}"''' at: which.
	text := self text.
	((startIndex > 1 and: [stopIndex <= text size])
			and: [ (text at: startIndex-1) = left and: [(text at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (String with: left), oldSelection string, (String with: right) attributes: self emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^true