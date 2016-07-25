method accessing
sourceCodeFor: aSelector 
	| class |
	class := self whoDefinesMethod: aSelector.
	class isNil ifTrue: [^nil].
	^(class methodFor: aSelector) source