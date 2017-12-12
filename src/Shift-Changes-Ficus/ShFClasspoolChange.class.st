Class {
	#name : #ShFClasspoolChange,
	#superclass : #ShFAbstractChange,
	#instVars : [
		'entryName'
	],
	#category : 'Shift-Changes-Ficus'
}

{ #category : #accessing }
ShFClasspoolChange >> addChangesFrom: oldDefinition to: newDefinition into: results [
	| newClassPoolEntries oldClassPoolEntries |
	newClassPoolEntries := newDefinition classPool asArray.
	oldClassPoolEntries := oldDefinition classPool asArray.
	
	newClassPoolEntries
		do: [ :new | 
			oldClassPoolEntries
				detect: [ :old | old = new ]
				ifNone: [ results
						add:
							(ShFClasspoolAdded new
								entryName: new;
								oldClass: oldDefinition;
								newClass: newDefinition;
								yourself) ] ].
						
	oldClassPoolEntries
		do: [ :old | 
			newClassPoolEntries
				detect: [ :new | old = new ]
				ifNone: [ results
						add:
							(ShFClasspoolRemoved new
								entryName: old;
								oldClass: oldDefinition;
								newClass: newDefinition;
								yourself) ] ]
]

{ #category : #accessing }
ShFClasspoolChange >> entryName [
	^ entryName
]

{ #category : #accessing }
ShFClasspoolChange >> entryName: anObject [
	entryName := anObject
]
