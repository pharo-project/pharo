initialization
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."

	UIManager default
		showWaitCursorWhile:			
			[ 
			| methodClass s |
			(methodClass := self methodClass) notNil
				ifTrue:
					[ 
					methodClass
						compile: self text
						classified: category
						withStamp: stamp
						notifying: nil ].
			type == #doIt
				ifTrue:
					[ 
					((s := self string) beginsWith: '----')
						ifFalse: [ self class compiler evaluate: s ] ].
			type == #classComment
				ifTrue:
					[ (Smalltalk globals at: class asSymbol) comment: self text stamp: stamp ] ]