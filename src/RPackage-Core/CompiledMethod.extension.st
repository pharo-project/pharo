Extension { #name : #CompiledMethod }

{ #category : #'*rpackage-core' }
CompiledMethod >> adoptToBeExtensionIn: aPackage [

	self properties at: #package put: aPackage
]

{ #category : #'*rpackage-core' }
CompiledMethod >> adoptToNotBeExtension [

	self properties removeKey: #package ifAbsent: [^self]
]

{ #category : #'*rpackage-core' }
CompiledMethod >> isDefinedInPackage: aPackage [
	^ aPackage includesDefinedSelector: self selector ofClass: self methodClass
]

{ #category : #'*rpackage-core' }
CompiledMethod >> isExtension [
	^ self origin package ~= self package
]

{ #category : #'*rpackage-core' }
CompiledMethod >> isExtensionInPackage: anRPackage [
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass
]

{ #category : #'*rpackage-core' }
CompiledMethod >> package [

	^self properties 
		at: #package 
		ifAbsent: [ self origin package]
]

{ #category : #'*rpackage-core' }
CompiledMethod >> package: aPackage [
	self isInstalled ifFalse: [
		self properties at: #package put: aPackage.
		^self].
	
	aPackage addMethod: self
]

{ #category : #'*rpackage-core' }
CompiledMethod >> packageFromOrganizer: anRPackageOrganizer [
	| originSelector |
	"This method returns the package this method belongs to.  
	It takes into account classes and traits.  
	If the method is in no package, returns nil by now"
	self flag: 'TODO: use anRPackageOrganizer, or better delegate to anRPackageOrganizer'.
	originSelector := self originMethod selector.
	
	^self origin packages 
		detect: [ :each | 
			self origin isMeta
				ifFalse: [ each includesSelector: originSelector ofClassName: self origin instanceSide originalName]
				ifTrue: [ each includesSelector: originSelector ofMetaclassName: self origin instanceSide originalName]] 
		ifNone: [ nil ]
]
