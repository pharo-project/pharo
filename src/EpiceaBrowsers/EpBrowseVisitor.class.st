"
I implement how to browse EpCodeChange instances.
"
Class {
	#name : 'EpBrowseVisitor',
	#superclass : 'EpLogBrowserOperationVisitor',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'visitor' }
EpBrowseVisitor >> visitBehaviorChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: #browse
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitCodeChange: aChange [
	^ self error: 'Should not be reached'
]

{ #category : 'visitor' }
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
					behavior browse ] ]
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitPackageChange: aChange [

	(self packageOrganizer packageNamed: aChange packageName ifAbsent: [ ^ self inform: 'Package ' , aChange packageName , ' not found.' ]) browse
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitPackageTagChange: aPackageTagChange [

	Smalltalk tools browser openOnPackage: ((self packageOrganizer
			  packageNamed: aPackageTagChange packageName
			  ifAbsent: [ ^ self inform: 'Package named ' , aPackageTagChange packageName , ' not found in the system' ])
			 tagNamed: aPackageTagChange tagName
			 ifAbsent: [ ^ self inform: 'Tag named ' , aPackageTagChange tagName , ' not found in package named ' , aPackageTagChange packageName ])
]
