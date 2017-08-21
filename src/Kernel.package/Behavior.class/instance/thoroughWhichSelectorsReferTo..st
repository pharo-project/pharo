testing method dictionary
thoroughWhichSelectorsReferTo: literal
	"Answer a set of selectors whose methods access the argument as a 
	literal. Dives into the compact literal notation, making it slow but 
	thorough "
	| specialIndex selectors |
	"for speed we check the special selectors here once per class"
	specialIndex := Smalltalk specialSelectorIndexOrNil: literal.
	selectors := OrderedCollection new.
	self selectorsAndMethodsDo: [ :sel :method |
			(method hasSelector: literal specialSelectorIndex: specialIndex) ifTrue: [selectors add: sel]].
	^ selectors