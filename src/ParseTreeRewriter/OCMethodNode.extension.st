Extension { #name : 'OCMethodNode' }

{ #category : '*ParseTreeRewriter' }
OCMethodNode >> removeSubtree: aTree [

	^ aTree isReturn
		ifTrue: [ OCParseTreeRewriter
					 replace: aTree formattedCode
					 with: '' in: self
					 onInterval: aTree sourceInterval ]
		ifFalse: [ OCParseTreeRewriter
					  replaceStatements: aTree formattedCode
		 			  with: '' in: self
					  onInterval: aTree sourceInterval ]
]
