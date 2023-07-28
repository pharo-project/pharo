Extension { #name : #RPackage }

{ #category : #'*Deprecated12' }
RPackage >> definedClassesDo: aBlock [

	self deprecated:
		'Use #definedClasses and a do instead because the name of this method is not explicit since it iterates over the *name* of the classes and not the classes themselves.'.
	^ self definedClassNames do: aBlock
]

{ #category : #'*Deprecated12' }
RPackage >> includesDefinedSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : #'*Deprecated12' }
RPackage >> includesDefinedSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Only checks methods defined (not extended by other packages or package extensions)"

	self deprecated: 'Use #includesDefinedSelector:ofClass: with the class side of the class directly'.
	^ self includesDefinedSelector: aSelector ofClass: (self environment at: aClassName) class
]

{ #category : #'*Deprecated12' }
RPackage >> includesSelector: aSelector ofClassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	self deprecated: 'Use #includesSelector:ofClass: directly with the class.'.
	^ self includesSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : #'*Deprecated12' }
RPackage >> includesSelector: aSelector ofMetaclassName: aClassName [
	"Return true if the receiver includes the method of selector aSelector. Checks methods defined locally as well as extending other packages"

	self deprecated: 'Use #includesSelector:ofClass: directly with the class.'.
	^ self includesSelector: aSelector ofClass: (self environment at: aClassName)
]

{ #category : #'*Deprecated12' }
RPackage >> removeClassNamed: aClassName [

	self deprecated: 'Use #removeClass: with a real class instead.'.
	^ self removeClass: (self organizer environment at: aClassName)
]
