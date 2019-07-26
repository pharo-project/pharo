Extension { #name : #RPackage }

{ #category : #'*System-Support' }
RPackage >> allUnsentMessages [
	^SystemNavigation new allUnsentMessagesIn: (self methods collect: [ :cm | cm selector ])
]
