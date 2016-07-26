*opalcompiler-core
isInlineCase
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineCase ifFalse: [ ^false ]]. 
	self isCascaded ifTrue: [^ false].
	(#(caseOf: caseOf:otherwise:) includes: self selector) ifFalse: [^ false].
	self arguments size = 2 ifTrue: [  "otherwise block"
		self arguments last isBlock ifFalse: [^ false]].
	self arguments first isArray ifFalse: [^ false].
	self arguments first statements do: [:assoc |
		(assoc isMessage and: [assoc selector == #->]) ifFalse: [^ false].
		assoc receiver isBlock ifFalse: [^ false].
		assoc receiver arguments isEmpty ifFalse: [^ false].
		assoc arguments first isBlock ifFalse: [^ false].
	].
	^ true