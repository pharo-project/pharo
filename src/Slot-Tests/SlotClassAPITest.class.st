Class {
	#name : #SlotClassAPITest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-Class API'
}

{ #category : #tests }
SlotClassAPITest >> testHasSpecificLayout [
	"A specific layout is a non fixed one (a fixed one is the default one you get when defining a class)"
	
	self deny: Point hasSpecificLayout.
	self assert: Array hasSpecificLayout
]
