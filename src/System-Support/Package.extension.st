Extension { #name : 'Package' }

{ #category : '*System-Support' }
Package >> allUnsentMessages [

	^ SystemNavigation new
		  environment: self environment;
		  allUnsentMessagesIn: (self methods collect: [ :cm | cm selector ]) asSet
]
