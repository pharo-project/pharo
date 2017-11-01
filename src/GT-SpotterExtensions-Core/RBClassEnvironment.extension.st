Extension { #name : #RBClassEnvironment }

{ #category : #'*GT-SpotterExtensions-Core-private' }
RBClassEnvironment >> withClassesFixedForNautilus [
	classes ifEmpty: [ 
		metaClasses do: [ :each | 
			classes add: each ] ]
]
