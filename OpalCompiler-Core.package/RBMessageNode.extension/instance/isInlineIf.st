*opalcompiler-core
isInlineIf
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineIf ifFalse: [ ^false ]]. 
	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	(#(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:) includes: self selector) ifFalse: [^ false].
	self arguments do: [:node | node isBlock ifFalse: [^ false]].
	self arguments do: [:block | block arguments isEmpty ifFalse: [^ false
		]
	].
	^ true