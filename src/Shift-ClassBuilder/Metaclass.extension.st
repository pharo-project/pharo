Extension { #name : 'Metaclass' }

{ #category : '*Shift-ClassBuilder' }
Metaclass >> << aMetaclass [
	"Return a builder of class FluidClassSideClassBuilder.
	To get a class, this builder should be sent #build. This way we can manipulate and test class definition without being forced to get them added to the environment."

	self assert: self soleInstance = aMetaclass soleInstance superclass.

	^ ShiftClassSideBuilder new
		  superclass: self soleInstance;
		  name: aMetaclass soleInstance name;
		  fillInstanceSideFromClass: aMetaclass soleInstance;
		  yourself
]

{ #category : '*Shift-ClassBuilder' }
Metaclass >> instanceVariableNames: instVarString [

	^ self slots: instVarString asSlotCollection
]
