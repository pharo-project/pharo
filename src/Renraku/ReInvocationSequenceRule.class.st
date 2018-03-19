"
I am checking for the violation in message sequences.

Subclass me and rewrite initialization method to all super initialize and then all the sequences with the methods for adding protocol.

Check the class side examples for examples.

You have to call
ReRuleManager reset
to make changes take place!!!!!!!!!!!!!!!!
"
Class {
	#name : #ReInvocationSequenceRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		'preConditions',
		'postConditions'
	],
	#category : #'Renraku-Rules'
}

{ #category : #example }
ReInvocationSequenceRule class >> checksNode [
	^ true
]

{ #category : #example }
ReInvocationSequenceRule class >> exampleSequenceMethods [
	<sampleInstance>
	<gtExample>
	<label: 'You should not do this. Instead subclass and call #add:… in the initialization method'>
	
	^ self new
		add: #initiallize requiresPreSend: #(basicNew new);
		add: #(setAsFixed setAsNotFixed) requiresPostSend: #(add: addAll:);
		add: #edges requiresPreSend: #nodes
]

{ #category : #'as yet unclassified' }
ReInvocationSequenceRule class >> isVisible [

	^ self name ~= #ReInvocationSequenceRule and: [ 
		super isVisible ]
]

{ #category : #compiling }
ReInvocationSequenceRule class >> noteCompilationOf: aSelector meta: isMeta [

	 aSelector == #initialize & isMeta not ifTrue: [ 
		ReRuleManager reset ]
]

{ #category : #adding }
ReInvocationSequenceRule >> add: preSelectors requiresPostSend: postSelectors [

	postConditions add: (
		ReInvocationSequenceRuleSequence
			before: preSelectors
			after: postSelectors)
]

{ #category : #adding }
ReInvocationSequenceRule >> add: postSelectors requiresPreSend: preSelectors [

	preConditions add: (
		ReInvocationSequenceRuleSequence
			before: preSelectors
			after: postSelectors)
]

{ #category : #running }
ReInvocationSequenceRule >> check: aNode forCritiquesDo: aCriticBlock [

	aNode isMethod ifFalse: [ ^ self ].
	
	(self  preCheck: aNode forCriticsDo: aCriticBlock) &  
	(self postCheck: aNode forCriticsDo: aCriticBlock)
]

{ #category : #'critic creation' }
ReInvocationSequenceRule >> critic: aCriticClass for: ast at: node regarding: sequence [

	^ aCriticClass 
		withAnchor: (
			ReIntervalSourceAnchor
				entity: ast
				interval: node sourceInterval)
		by: self
		method: node selector
		condition: sequence
		
]

{ #category : #initialization }
ReInvocationSequenceRule >> initialize [
	super initialize.
	
	preConditions := Set new.
	postConditions := Set new.
]

{ #category : #accessing }
ReInvocationSequenceRule >> name [

	^ 'invocation order incorect'
]

{ #category : #running }
ReInvocationSequenceRule >> postCheck: anAST forCriticsDo: aCriticBlock [
	| remainingPostConditions encounteredCritics |	
	postConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPostConditions := postConditions copy.
	
	anAST reverseNodesDo: [ :node |
		
		remainingPostConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			
			"remove the matched conditions while criticizing before-matches"
			remainingPostConditions removeAllSuchThat: [ :cond |
				| beforeMatched |
				(beforeMatched := cond beforeMatches: node selector) ifTrue: [ 
					
					encounteredCritics := true.
					aCriticBlock cull: (
						self postCriticFor: anAST at: node regarding: cond) ].
				 "if the following expression is true, the condition will be removed"
				beforeMatched or: [ cond afterMatches: node selector] ] ] ].
					

	^ encounteredCritics
]

{ #category : #'critic creation' }
ReInvocationSequenceRule >> postCriticFor: ast at: node regarding: sequence [

	^ self critic: ReLaterInvocationCritique for: ast at: node regarding: sequence 
]

{ #category : #running }
ReInvocationSequenceRule >> preCheck: anAST forCriticsDo: aCriticBlock [
	| remainingPreConditions encounteredCritics |	
		
	preConditions ifEmpty: [ ^ false ].
	encounteredCritics := false.
	
	remainingPreConditions := preConditions copy.
	
	anAST nodesDo: [ :node |
		remainingPreConditions ifEmpty: [ ^ encounteredCritics ].
		node isMessage ifTrue: [ 
			
			"remove the matched conditions while criticizing after-matches"
			remainingPreConditions removeAllSuchThat:  [ :cond |
				| afterMatched |
				(afterMatched := cond afterMatches: node selector) ifTrue: [ 
					encounteredCritics := true.
					aCriticBlock cull: (
						self preCriticFor: anAST at: node regarding: cond) ].
				 
				"if the following expression is true, the condition will be removed"
				afterMatched or: [ cond beforeMatches: node selector] ] ] ].
					

	^ encounteredCritics
]

{ #category : #'critic creation' }
ReInvocationSequenceRule >> preCriticFor: ast at: node regarding: sequence [

	^ self critic: RePriorInvocationCritique for: ast at: node regarding: sequence 
]
