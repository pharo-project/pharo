accessing
packageDefiningOrExtendingSelector: aSelector inClassNamed: aClassNameSymbol
	"this implementation is slower
		aClass packages detect: [:each | each includesSelector: aSelector ofClass: aClass ]"
	| classPackage |
	
	classPackage := self packageOfClassNamed: aClassNameSymbol.
	classPackage ifNil: [ ^ nil ].
	(classPackage includesSelector: aSelector ofClassName: aClassNameSymbol)
		ifTrue: [ ^classPackage ].
		
	^(self extendingPackagesOfClassNamed: aClassNameSymbol)
		detect: [ :p | p includesSelector: aSelector ofClassName: aClassNameSymbol ]
		ifNone: [ nil ]