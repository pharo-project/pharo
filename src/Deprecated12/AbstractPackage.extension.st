Extension { #name : #AbstractPackage }

{ #category : #'*Deprecated12' }
AbstractPackage >> includesDefinedSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : #'*Deprecated12' }
AbstractPackage >> includesDefinedSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class side of the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName) class
]
