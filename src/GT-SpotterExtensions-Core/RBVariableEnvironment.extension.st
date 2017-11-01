Extension { #name : #RBVariableEnvironment }

{ #category : #'*GT-SpotterExtensions-Core-private' }
RBVariableEnvironment >> withClassesFixedForNautilus [
	instanceVariables ifEmpty: [ 
		classVariables keysAndValuesDo: [ :class :variables | 
			instanceVariables at: class ifAbsentPut: [ Set new ] ] ].

]
