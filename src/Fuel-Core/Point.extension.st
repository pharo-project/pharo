Extension { #name : #Point }

{ #category : #'*Fuel-Core' }
Point >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitPoint: self

]
