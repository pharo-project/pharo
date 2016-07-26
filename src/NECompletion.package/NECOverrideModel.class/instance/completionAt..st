action
completionAt: aNumber 
	| output source declaration |
	source :=  (self methodAt: aNumber) sourceCode.
	declaration := (source lineCorrespondingToIndex: 1) trimRight.
	output := WriteStream on: String new.
	output nextPutAll: declaration; cr; tab.
	(source includesSubstring: 'subclassResponsibility') ifFalse: [
		(source includes: $^)
			ifTrue: [ output nextPutAll: '^ ' ].
		output
			nextPutAll: 'super ';
			nextPutAll: declaration ].
	^ output contents