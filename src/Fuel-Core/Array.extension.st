Extension { #name : #Array }

{ #category : #'*Fuel-Core' }
Array >> fuelAccept: aGeneralMapper [
	
	"This IF is because there are subclasses of Array that are weak. If we do not put this IF, all subclasses fuel be using the #visitVariableObject:, loosing the capability of the weak
	However, this method is just to optimize the method lookup of the message #fuelAccept:. If this method is removed, the default behavior of Object shuld work as well. If the performance is not that different, then we can remove this method which has an ugly if."
	^ self class isWeak
		ifTrue: [ aGeneralMapper visitWeakObject: self ]
		ifFalse: [ aGeneralMapper visitVariableObject: self ]
	
	
]

{ #category : #'*Fuel-Core' }
Array >> fuelReplacement [
	^ self == Smalltalk specialObjectsArray
		ifTrue: [ nil ]
		ifFalse: [ self ]
]
