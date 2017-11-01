Extension { #name : #Point }

{ #category : #'*Fuel' }
Point >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitPoint: self

]
