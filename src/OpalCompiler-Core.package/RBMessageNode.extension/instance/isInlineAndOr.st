*opalcompiler-core
isInlineAndOr
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineAndOr ifFalse: [ ^false ]]. 
	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	(#(and: or:) includes: self selector) ifFalse: [^ false].
	(self arguments allSatisfy: [ :each | each isBlock ])  ifFalse: [^ false].
	(self arguments allSatisfy: [ :each | each arguments isEmpty ]) ifFalse: [^ false ].
	^ true