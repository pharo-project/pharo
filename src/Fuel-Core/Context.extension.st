Extension { #name : #Context }

{ #category : #'*Fuel-Core' }
Context >> cleanCopy [ 

	^ self class 
		sender: nil 
		receiver: receiver 
		method: method
		arguments: #()
]

{ #category : #'*Fuel-Core' }
Context >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitMethodContext: self
]

{ #category : #'*Fuel-Core' }
Context class >> newFromFrameSize: aFrameSize [
	
	^ super basicNew: aFrameSize
]
