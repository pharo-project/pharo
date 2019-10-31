Extension { #name : #RBPragmaNode }

{ #category : #'*NECompletion' }
RBPragmaNode >> completionEntries [
		^ self select: Symbol allSymbols beginningWith: self selector
]
