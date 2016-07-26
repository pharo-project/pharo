accessing
packageDefiningOrExtendingSelector: aSelector inClass: aClass
	"this implementation is slower
		aClass packages detect: [:each | each includesSelector: aSelector ofClass: aClass ]"

	
	^ (aClass packages) detect: [:each | each includesSelector: aSelector ofClass: aClass ] ifNone: [
		"if we do not find in the packages of the class, it means that the method is coming from a trait: "
		|tmpTrait|
		tmpTrait := (aClass traitComposition traitProvidingSelector: aSelector).
		(aClass traitComposition traitProvidingSelector: aSelector) packages detect: [:each |
			each includesSelector: aSelector ofClass: tmpTrait.   
			].
		]
	
"	self packages detect: [:each | 
		each includesSelector: aSelector ofClass: aClass ].
"	
	