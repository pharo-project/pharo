running
basicCheck: aMethod 
	aMethod isFromTrait
		ifTrue: [ ^ false ].
	(self rewriteRule canMatchMethod: aMethod) 
		ifFalse: [ ^ false ].
	class := aMethod methodClass.
	^ self rewriteRule executeTree: aMethod ast copy