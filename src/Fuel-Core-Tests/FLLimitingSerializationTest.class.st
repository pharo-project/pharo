Class {
	#name : 'FLLimitingSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Configuration',
	#package : 'Fuel-Core-Tests',
	#tag : 'Configuration'
}

{ #category : 'tests' }
FLLimitingSerializationTest >> testDepthAndObjectLimit [
	| root pair materialized |
	root := FLPair new.
	pair := root.
	1
		to: 10
		do: [ :index |
			pair
				left: FLPair new;
				right: (1 to: 10) asArray.
			pair := pair left ].
	
	self serializer
		object: root;
		limitDepthTo: 3;
		limitObjectsTo: 5;
		serialize.
		
	materialized := self materialized.
	self assert: materialized left isNil.
	self assertCollection: materialized right equals: #(nil nil nil nil nil nil nil 8 9 10).
]

{ #category : 'tests' }
FLLimitingSerializationTest >> testDepthLimit [
	| root pair materialized |
	root := FLPair new.
	pair := root.
	1
		to: 10
		do: [ :index |
			pair
				left: FLPair new;
				right: index.
			pair := pair left ].
	
	self serializer
		object: root;
		limitDepthTo: 3;
		serialize.
		
	materialized := self materialized.
	self assert: materialized left isNotNil.
	self assert: materialized right equals: 1.
	self assert: materialized left left isNotNil.
	self assert: materialized left right equals: 2.
	self assert: materialized left left left isNil.
	self assert: materialized left left right isNil
]

{ #category : 'tests' }
FLLimitingSerializationTest >> testEnableObjectsLimitWarning [
	| root materialized |
	root := FLPair new
		left: (FLPair new
			left: 2;
			right: 3;
			yourself);
		right: 1;
		yourself.
	
	self serializer
		object: root;
		limitObjectsTo: 3;
		enableLimitObjectsException.
		
	self 
		should: [ self serializer serialize ]
		raise: FLObjectLimitExceededWarning.
	
	[ self serializer serialize ]
		on: FLObjectLimitExceededWarning
		do: [ :ex | ex resume ].
		
	materialized := self materialized.
	self assert: materialized left isNotNil.
	self assert: materialized right equals: 1.
	self assert: materialized left left isNil.
	self assert: materialized left right isNil
]

{ #category : 'tests' }
FLLimitingSerializationTest >> testEnableObjectsLimitWarningWithDepthLimit [
	| root |
	root := FLPair new
		left: (FLPair new
			left: 2;
			right: 3;
			yourself);
		right: 1;
		yourself.
	
	self serializer
		object: root;
		limitObjectsTo: 3;
		limitDepthTo: 5;
		enableLimitObjectsException.
		
	self 
		should: [ self serializer serialize ]
		raise: FLObjectLimitExceededWarning.
		
	self serializer
		limitObjectsTo: 3;
		limitDepthTo: 1;
		enableLimitObjectsException.
		
	self 
		shouldnt: [ self serializer serialize ]
		raise: FLObjectLimitExceededWarning
]

{ #category : 'tests' }
FLLimitingSerializationTest >> testObjectLimit [
	| root pair materialized |
	root := FLPair new.
	pair := root.
	1
		to: 10
		do: [ :index |
			pair
				left: FLPair new;
				right: index.
			pair := pair left ].
	
	self serializer
		object: root;
		limitObjectsTo: 4;
		serialize.
		
	materialized := self materialized.
	self assert: materialized left isNotNil.
	self assert: materialized right equals: 1.
	self assert: materialized left left isNil.
	self assert: materialized left right equals: 2
]
