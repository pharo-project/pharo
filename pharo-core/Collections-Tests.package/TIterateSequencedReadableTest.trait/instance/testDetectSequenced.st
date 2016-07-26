tests - iterate on sequenced reable collections
testDetectSequenced
" testing that detect keep the first element returning true for sequenceable collections "

	| element result |
	element := self nonEmptyMoreThan1Element   at:1.
	result:=self nonEmptyMoreThan1Element  detect: [:each | each notNil ].
	self assert: result = element. 