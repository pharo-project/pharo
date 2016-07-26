running
basicCheck: aMethod
	| class |
	class := aMethod methodClass theNonMetaClass.
	^ aMethod hasLiteral: (class environment associationAt: class name ifAbsent: [ ^ false ])
	