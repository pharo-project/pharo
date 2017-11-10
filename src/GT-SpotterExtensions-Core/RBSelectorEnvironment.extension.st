Extension { #name : #RBSelectorEnvironment }

{ #category : #'*GT-SpotterExtensions-Core-private' }
RBSelectorEnvironment >> withClassesFixedForNautilus [
	classSelectors ifEmpty: [ 
		metaClassSelectors keysAndValuesDo: [ :class :selectors | 
			classSelectors at: class ifAbsentPut: [ IdentitySet new ] ] ]
]
