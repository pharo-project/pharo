system v abi
combineSysVAMD64RegisterClass: leftClass with: rightClass
	"AMD64 SystemV eightbyte classification reduction"
	
	"Same class."
	leftClass == rightClass ifTrue: [ ^ leftClass ].
	
	"If one has no class, use the other."
	leftClass isNil ifTrue: [ ^ rightClass ].
	rightClass isNil ifTrue: [ ^ leftClass ].
	
	"If one is memory, the result is memory."
	(leftClass == #memory or: [ rightClass == #memory ]) ifTrue: [ ^ #memory ].
	
	"If one is integer, the result is integer."
	(leftClass == #integer or: [ rightClass == #integer ]) ifTrue: [ ^ #integer ].
	
	"TODO: If one of the classes is X87, X87UP, COMPLEX_X87 class, #memory is used as class."
	
	"Otherwise, use the SSE class"
	^ #float