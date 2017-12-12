Class {
	#name : #ShFSharedPoolChange,
	#superclass : #ShFAbstractChange,
	#instVars : [
		'poolName'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFSharedPoolChange >> addChangesFrom: oldDefinition to: newDefinition into: results [
	| newSharedPools oldSharedPools |
	newSharedPools := newDefinition sharedPools asArray.
	oldSharedPools := oldDefinition sharedPools asArray.
	
	newSharedPools
		do: [ :new | 
			oldSharedPools
				detect: [ :old | old = new ]
				ifNone: [ results
						add:
							(ShFSharedPoolAdded new
								poolName: new;
								oldClass: oldDefinition;
								newClass: newDefinition;
								yourself) ] ].
						
	oldSharedPools
		do: [ :old | 
			newSharedPools
				detect: [ :new | old = new ]
				ifNone: [ results
						add:
							(ShFSharedPoolRemoved new
								poolName: old;
								oldClass: oldDefinition;
								newClass: newDefinition;
								yourself) ] ]
]

{ #category : #accessing }
ShFSharedPoolChange >> poolName [
	^ poolName
]

{ #category : #accessing }
ShFSharedPoolChange >> poolName: anObject [
	poolName := anObject
]
