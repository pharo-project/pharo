"
I signal when there is need to migrate the instances. 
Of course if I migrate the instances, this change is propagated to the subclasses.
I propagate the changes adding a change like me in the subclasses.
"
Class {
	#name : #ShInstanceShapeChanged,
	#superclass : #ShAbstractChange,
	#category : 'Shift-Changes'
}

{ #category : #testing }
ShInstanceShapeChanged >> hasToMigrateInstances [
	^ true.
]

{ #category : #testing }
ShInstanceShapeChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder changes add: self.
]
