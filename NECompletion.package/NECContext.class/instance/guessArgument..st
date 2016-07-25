private-type-guessing
guessArgument: aSHRange 
	| name |
	name := self sourceOf: aSHRange.
	(name = 'html') ifTrue: [
		 Smalltalk at: #WAHtmlCanvas ifPresent: [ :canvasClass | ^canvasClass].
		 Smalltalk at: #WARenderCanvas ifPresent: [ :canvasClass | ^canvasClass]].
	^ NECInstVarTypeGuesser getClassFromTypeSuggestingName: name