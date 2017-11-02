Extension { #name : #CompiledMethod }

{ #category : #'*rpackage-core' }
CompiledMethod >> isDefinedInPackage: anRPackage [
	^ anRPackage includesDefinedSelector: self selector ofClass: self methodClass
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
	^ self packageFromOrganizer: RPackage organizer
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
				ifFalse: [ each includesSelector: originSelector ofClassName: self origin theNonMetaClass originalName]
				ifTrue: [ each includesSelector: originSelector ofMetaclassName: self origin theNonMetaClass originalName]] 
		ifNone: [ nil ]
]
