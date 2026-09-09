Class {
	#name : 'MetacelloReferenceTest',
	#superclass : 'TestCase',
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'testing' }
MetacelloReferenceTest >> testSmokeTest [
	"We should add much more assertions on the produced version."

	self shouldnt: [ MetacelloReferenceBaseline project version ] raise: Error
]
