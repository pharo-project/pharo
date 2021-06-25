Trait {
	#name : #RBTTransformationModel,
	#instVars : [
		'model'
	],
	#category : #'Refactoring2-Transformations-Model-Migration'
}

{ #category : #default }
RBTTransformationModel >> defaultEnvironment [

	^ RBBrowserEnvironment new
]

{ #category : #accessing }
RBTTransformationModel >> model [

	^ model ifNil: [ model := self newModel ]
]

{ #category : #accessing }
RBTTransformationModel >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : #'instance creation' }
RBTTransformationModel >> newModel [

	^ (RBNamespace2
		onEnvironment: self defaultEnvironment)
		name: self printString;
		yourself
]

{ #category : #transforming }
RBTTransformationModel >> poolVariableNamesFor: aClass [ 
	| pools |
	pools := Set new.
	aClass withAllSuperclasses do: 
			[:each | 
			each allPoolDictionaryNames do: 
					[:pool |
					pools addAll: (self poolVariableNamesIn: pool asSymbol) ] ].
	^pools
]

{ #category : #transforming }
RBTTransformationModel >> poolVariableNamesIn: poolName [ 
	^(model classNamed: poolName) classPool keys
]
