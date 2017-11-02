Extension { #name : #Context }

{ #category : #'*Fuel' }
Context >> cleanCopy [ 

	^ self class 
		sender: nil 
		receiver: receiver 
		method: method
		arguments: #()
]

{ #category : #'*Fuel' }
Context >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitMethodContext: self
]

{ #category : #'*Fuel' }
Context class >> newFromFrameSize: aFrameSize [
	
	^ super basicNew: aFrameSize
]
