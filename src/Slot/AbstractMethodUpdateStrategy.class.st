"
I am used to update compiled methods in response to class modifications.
My sublcasses implement different strategies to update affected methods.
"
Class {
	#name : #AbstractMethodUpdateStrategy,
	#superclass : #Object,
	#category : 'Slot-ClassBuilder'
}

{ #category : #updating }
AbstractMethodUpdateStrategy >> transform: oldClass to: newClass using: aMethodModification [

	self subclassResponsibility 
]
