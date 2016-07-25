naming
setNameTo: aName 
	| nameToUse nameString |
	nameToUse := aName ifNotNil: 
					[(nameString := aName asString) notEmpty ifTrue: [nameString] ifFalse: ['*']].
	self setNamePropertyTo: nameToUse	"no Texts here!"