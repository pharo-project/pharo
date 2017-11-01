Class {
	#name : #ReVarSearchSourceAnchorTest,
	#superclass : #TestCase,
	#category : 'Renraku-Test'
}

{ #category : #tests }
ReVarSearchSourceAnchorTest >> testModifyFor [
	| anchor |
	anchor := ReVarSearchSourceAnchor new.
	
	self
		assert: (anchor modify: 'one' for: #(one))
		equals: '''one'''.
	
	self
		assert: (anchor modify: 'one' for: #(one two))
		equals: '''one '.
	
	self
		assert: (anchor modify: 'two' for: #(one two))
		equals: ' two'''.
	
	self
		assert: (anchor modify: 'two' for: #(one two three))
		equals: ' two '
]

{ #category : #tests }
ReVarSearchSourceAnchorTest >> testSearchString [

	| anchor |
	
	anchor := ReVarSearchSourceAnchor
		entity: Object newAnonymousSubclass
		string: 'var'.
		
	self
		assert: anchor searchString
		equals: 'var'
]

{ #category : #tests }
ReVarSearchSourceAnchorTest >> testSearchString2 [

	| anchor |
	
	anchor := ReVarSearchSourceAnchor
		entity: (
			Object newAnonymousSubclass
				addClassVarNamed: 'Var';
				yourself)
		string: 'Var'.
		
	self
		assert: anchor searchString
		equals: '''Var'''
]
