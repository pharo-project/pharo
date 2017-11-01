Extension { #name : #MethodGroup }

{ #category : #'*Nautilus' }
MethodGroup >> restrict: anEnvironment [
	^ anEnvironment selectMethods:[:m | self methods includes: m]
]
