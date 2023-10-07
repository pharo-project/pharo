Extension { #name : 'RPackage' }

{ #category : '*System-Support' }
RPackage >> allUnsentMessages [

	^ SystemNavigation new
		  environment: self environment;
		  allUnsentMessagesIn: (self methods collect: [ :cm | cm selector ]) asSet
]
