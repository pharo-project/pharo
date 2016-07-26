tests extending
stringCharacterInCollPackage
	| p |
	p := self packageClass named: #CollPackage.
	p addClassDefinition: String.
	p addClassDefinition: Character.
	p addMethod: String>>#indexOf:.
	p addMethod: String>>#isAllDigits.
	p addMethod: String class>>#cr.
	p addMethod: Character>>#asInteger.
	p addMethod: Text>>#allBold.
	^ p