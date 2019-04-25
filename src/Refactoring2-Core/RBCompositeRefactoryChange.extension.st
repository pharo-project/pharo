Extension { #name : #RBCompositeRefactoryChange }

{ #category : #'*Refactoring2-Core' }
RBCompositeRefactoryChange >> addProtocolNamed: aString in: aClass [ 

	^ self addChange: (RBAddProtocolChange addProtocolNamed: aString in: aClass)
]

{ #category : #'*Refactoring2-Core' }
RBCompositeRefactoryChange >> category: aString for: aClass [ 

	^ self addChange: (RBClassCategoryChange category: aString for: aClass)
]

{ #category : #'*Refactoring2-Core' }
RBCompositeRefactoryChange >> removeProtocolNamed: aString in: aClass [ 
	self flag: #todo. 
	"This change is not made via the changefactory!!!"
	^ self addChange: (RBRemoveProtocolChange removeProtocolNamed: aString in: aClass)
]

{ #category : #'*Refactoring2-Core' }
RBCompositeRefactoryChange >> selector: aSelector in: aClass classified: aProtocol [
	self flag: #todo. 
	"This change is not made via the changefactory!!!"
	^ self addChange: (RBMethodProtocolChange
							selector: aSelector
							in: aClass
							classified: aProtocol)
]
