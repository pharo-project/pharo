private - utilities
chooseFrom: aList lines: linesArray title: queryString
	"Choose an item from the given list. Answer the index of the selected item. Do not invoke this method - it is here to help in case of mini system, better use UIManager default"
	
	
	"MenuMorph
		chooseFrom: #('Hello' 'Pharoers' 'Here' 'We' 'Go')
		lines: #(2 4)
		title: 'What''s up?'"
		
	| menu result |
	(ProvideAnswerNotification signal: queryString) ifNotNil:[:answer | 
		1 to: aList size do:[:i| (aList at: i) = answer ifTrue:[^i]].
		^0].
	result := 0.
	menu := self new.
	menu addTitle: queryString.
	1 to: aList size do:[:i| 
		menu add: (aList at: i) asString target: [:v| result := v] selector: #value: argument: i.
		(linesArray includes: i) ifTrue: [menu addLine]].
	
	menu invokeAt: ActiveHand position in: ActiveWorld allowKeyboard: true.
	^result