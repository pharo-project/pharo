Extension { #name : #RBNamespace }

{ #category : #'*Refactoring2-Core' }
RBNamespace >> category: aString for: aClass [

	^ changes addChange: (RBClassCategoryChange category: aString for: aClass)
]
