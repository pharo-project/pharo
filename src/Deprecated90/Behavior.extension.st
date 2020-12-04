Extension { #name : #Behavior }

{ #category : #'*Deprecated90' }
Behavior >> addSelector: selector withMethod: compiledMethod notifying: requestor [
	self
		deprecated: 'Please use #addSelector:withMethod: instead'
		transformWith:
			'`@receiver addSelector: `@statements1 withMethod: `@statements2 notifying: `@statements3'
		   -> '`@receiver addSelector: `@statements1 withMethod: `@statements2'.
	^ self addSelector: selector withMethod: compiledMethod
]

{ #category : #'*Deprecated90' }
Behavior >> basicAddSelector: selector withMethod: compiledMethod [
	self
		deprecated: 'Please use #addSelectorSilently:withMethod: instead'
		transformWith:
			'`@receiver basicAddSelector: `@statements1 withMethod: `@statements2'
		   -> '`@receiver addSelectorSilently: `@statements1 withMethod: `@statements2'.
	self addSelectorSilently: selector withMethod: compiledMethod
]

{ #category : #'*Deprecated90' }
Behavior >> commentsAt:  selector [
	self 
		deprecated: 'use #comments on the method instead' 
		transformWith: '`@receiver commentsAt: `@argument'
						-> '(`@receiver compiledMethodAt: `@argument) comments'.
	
	^(self compiledMethodAt: selector) comments.
	

]

{ #category : #'*Deprecated90' }
Behavior >> firstCommentAt:  selector [
	
	self 
		deprecated: 'use #firstComment on the method instead' 
		transformWith: '`@receiver commentsAt: `@argument'
						-> '(`@receiver compiledMethodAt: `@argument) firstComment'.
	
	^(self compiledMethodAt: selector) firstComment
]

{ #category : #'*Deprecated90' }
Behavior >> firstPrecodeCommentFor:  selector [
	self 
		deprecated: 'use #comment on the method' 
		transformWith: '`@receiver firstPrecodeCommentFor: `@arg' -> '(`@receiver>>`@arg) comment'.
	^(self>>selector) comment
	
]

{ #category : #'*Deprecated90' }
Behavior >> precodeCommentOrInheritedCommentFor: selector [ 
	"Answer a string representing the first comment in the method associated 
	with selector, considering however only comments that occur before the 
	beginning of the actual code. If the version recorded in the receiver is 
	uncommented, look up the inheritance chain. Return nil if none found."
	
	| aSuper aComment |
	self deprecated: 'No senders, not API: will be removed in Pharo 10'.
	
	^ (aComment := self firstPrecodeCommentFor: selector) isEmptyOrNil
		ifTrue: [(self == Behavior
					or: [self superclass == nil
							or: [(aSuper := self superclass whichClassIncludesSelector: selector) == nil]])
				ifFalse: [aSuper precodeCommentOrInheritedCommentFor: selector]]
		ifFalse: [aComment]
]

{ #category : #'*Deprecated90' }
Behavior >> supermostPrecodeCommentFor: selector [ 
	"Answer a string representing the precode comment in the most distant 
	superclass's implementation of the selector. Return nil if none found."
	| aSuper superComment |
	self deprecated: 'No senders, not API: will be removed in Pharo 10'.
	(self == Behavior
			or: [self superclass == nil
					or: [(aSuper := self superclass whichClassIncludesSelector: selector) == nil]])
		ifFalse: ["There is a super implementor"
			superComment := aSuper supermostPrecodeCommentFor: selector].
	^ superComment
		ifNil: [self firstPrecodeCommentFor: selector
			"ActorState supermostPrecodeCommentFor: #printOn:"]
]
