Class {
	#name : 'RBDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'tests' }
RBDriverTest >> testAllDriversHaveADisplayString [

	self shouldnt: [ ReInteractionDriver drivers collect: [ :driver | driver driverName ] ] raise: Error.
	ReInteractionDriver drivers collect: [ :driver |
			self assert: driver driverName isString.
			self assert: driver driverName isNotEmpty ]
]
