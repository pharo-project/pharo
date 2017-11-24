"
I am an abstract class to silence certain parts of the system while running tests in my subclasses.

Currently this class silences the SystemAnnouncer and sets the Author to the class name when a test is run. 

"
Class {
	#name : #SlotSilentTest,
	#superclass : #SlotClassBuilderTest,
	#category : #'Slot-Tests'
}

{ #category : #running }
SlotSilentTest >> runCase [

	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		Author useAuthor: self class name asString during: [ super runCase ]
	]

]
