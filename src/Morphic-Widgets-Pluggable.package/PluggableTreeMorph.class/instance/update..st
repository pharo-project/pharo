updating
update: what
	what ifNil:[^self].
	what == getRootsSelector ifTrue:[
		self roots: (model perform: getRootsSelector)
	].
	what == getSelectedPathSelector ifTrue:[
		^self selectPath: (model perform: getSelectedPathSelector)
			in: (scroller submorphs at: 1 ifAbsent: [^self]) 
	].
	^super update: what