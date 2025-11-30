Extension { #name : 'ReAbstractTransformation' }

{ #category : '*Deprecated14' }
ReAbstractTransformation >> emptyCondition [
	self deprecated: 'Use trueCondition' transformWith: '`@rec emptyCondition' -> '`@rec trueCondition'. 
	^ RBCondition true
]

{ #category : '*Deprecated14' }
ReAbstractTransformation >> transform [
	"Do the actual operations."
	self deprecated: 'Use generateChanges or privateTransform instead. Check subclasses for more details.'. 
	
	self subclassResponsibility
]
