Morphic-Base-Widgets
beginsWith: aString fromList: aMorph
	| string |
	string := self userString ifNil: [(self submorphs collect: [:m | m userString]) detect: [:us | us notNil] ifNone: ['']].
	
	^ string asString beginsWith: aString fromList: aMorph