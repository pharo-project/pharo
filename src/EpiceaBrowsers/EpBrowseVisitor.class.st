"
I implement how to browse EpCodeChange instances.
"
Class {
	#name : #EpBrowseVisitor,
	#superclass : #EpLogBrowserOperationVisitor,
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #visitor }
EpBrowseVisitor >> visitBehaviorChange: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName 
		ifPresent: #browse
]

{ #category : #visitor }
EpBrowseVisitor >> visitCategoryChange: aChange [

	| package |
	package := RPackageOrganizer default packageNamed: aChange affectedPackageName.
	package classTags
		detect: [ :tag |
			tag categoryName = aChange categoryName
		]
		ifFound: [ :tag | Smalltalk tools browser openOnPackage: tag ]
		ifNone: [ self inform: 'Category ' , aChange categoryName , ' not found.' ].
]

{ #category : #visitor }
EpBrowseVisitor >> visitCodeChange: aChange [
	^ self error: 'Should not be reached'.
]

{ #category : #visitor }
EpBrowseVisitor >> visitMethodChange: aChange [

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior 
				compiledMethodAt: aChange methodAffectedSelector 
				ifPresent: #browse 
				ifAbsent: [ 
					self inform: 
						'Cannot find ', behavior asString, '>>', aChange methodAffectedSelector.
					behavior browse ] ].
]
