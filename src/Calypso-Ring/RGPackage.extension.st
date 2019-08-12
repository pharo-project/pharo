Extension { #name : #RGPackage }

{ #category : #'*Calypso-Ring' }
RGPackage >> asPackageName [

	^ self name
]

{ #category : #'*Calypso-Ring' }
RGPackage >> calypsoEnvironmentType [
	^RPackage
]

{ #category : #'*Calypso-Ring' }
RGPackage >> classTags [

	self isRingResolved not ifTrue: [ ^ '' ].
	
	 ^ self classes collect: [:aClass | (aClass category ifNil: [ '' ]) withoutPrefix: (self name, '-')] as: Set
]

{ #category : #'*Calypso-Ring' }
RGPackage >> classesForClassTag: aTag [

	^ self classes select: [ :each | each category  = aTag or: [ each category = (self packageName, '-', aTag) ] ]
]

{ #category : #'*Calypso-Ring' }
RGPackage >> extendedClassNames [

	^ self extendedClasses collect: #name
]

{ #category : #'*Calypso-Ring' }
RGPackage >> extendsClass: aClass [

	^ self extendedClasses includes: aClass.
]

{ #category : #'*Calypso-Ring' }
RGPackage >> extensionSelectorsForClass: aClass [

	^ (self extensionMethods select: [ :each | each parent = aClass ]) collect: #name
]

{ #category : #'*Calypso-Ring' }
RGPackage >> importClasses: classCollection [
	classCollection do: [ :each | 
		each package: self. ]
]

{ #category : #'*Calypso-Ring' }
RGPackage >> includesClassesAffectedBy: aSystemAnnouncement [

	^(aSystemAnnouncement affectsClassesDefinedInPackage: self)
		or: [ aSystemAnnouncement affectsClassesExtendedInPackage: self ]
]

{ #category : #'*Calypso-Ring' }
RGPackage >> isAboutPackage: aPackage [

	^self == aPackage
]

{ #category : #'*Calypso-Ring' }
RGPackage >> isEmpty [

	^ self definedClasses isEmpty and: [ self extensionMethods isEmpty ]
]

{ #category : #'*Calypso-Ring' }
RGPackage >> packageManifestOrNil [

	^ nil "TODO"
]

{ #category : #'*Calypso-Ring' }
RGPackage >> packageName [

	^ self name
]

{ #category : #'*Calypso-Ring' }
RGPackage >> removeFromSystem [

	self environment removePackage: self.
]

{ #category : #'*Calypso-Ring' }
RGPackage >> renameTo: aString [

	self name: aString
]
