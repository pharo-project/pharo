defaults
defaultValue 
	| font |
	
	font := TextStyle defaultFont. 
	
	(font isKindOf: StrikeFont) ifTrue: [ 
		"no strike fonts please"
"		^  LogicalFont familyName: 'Verdana' pointSize: 12. "
		
		^  LogicalFont familyName: 'Helvetica' pointSize: 12. 
		 ].
	^ font