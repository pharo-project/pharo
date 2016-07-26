installing
usesSpecialVariables
	"this checks if any ivar or class var is using more than just standard definitions.
	Complex vars are encoded with a string that starts with a # or one that has a space"
	
	^self variables anySatisfy: [:var | (var name beginsWith:'#') or: [ var name includes: Character space ]]