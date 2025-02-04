"
I implement how to browse EpCodeChange instances.
"
Class {
	#name : 'EpBrowseVisitor',
	#superclass : 'EpLogBrowserOperationVisitor',
	#traits : 'EpTCodeChangeVisitor',
	#classTraits : 'EpTCodeChangeVisitor classTrait',
	#instVars : [
		'application'
	],
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'accessing' }
EpBrowseVisitor >> application: anApplication [ 
	application := anApplication
]

{ #category : 'visitor' }
EpBrowseVisitor >> browseClass: aClass [

	application tools browser openOnClass: aClass
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitBehaviorChange: aChange [

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :p | self browseClass: p ]
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitCodeChange: aChange [
	^ self error: 'Should not be reached'
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitMethodChange: aChange [

	self behaviorNamed: aChange behaviorAffectedName ifPresent: [ :behavior |
		behavior
			compiledMethodAt: aChange methodAffectedSelector
			ifPresent: [ :method | application tools browser openOnMethod: method ]
			ifAbsent: [ application inform: 'Cannot find ' , behavior asString , '>>' , aChange methodAffectedSelector.
				self browseClass: behavior ] ]
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitPackageChange: aChange [

	| package |
	package := self packageOrganizer 
					packageNamed: aChange packageName 
					ifAbsent: [ ^ application inform: 'Package ' , aChange packageName , ' not found.' ].
	application tools browser openOnPackage: package 
]

{ #category : 'visitor' }
EpBrowseVisitor >> visitPackageTagChange: aPackageTagChange [
	
	| package tag |
	package := self packageOrganizer
		packageNamed: aPackageTagChange packageName
		ifAbsent: [ ^ application inform: 'Package named ' , aPackageTagChange packageName , ' not found in the system' ].
	tag := package
		tagNamed: aPackageTagChange tagName
		ifAbsent: [ ^ application inform: 'Tag named ' , aPackageTagChange tagName , ' not found in package named ' , aPackageTagChange packageName ].
			
	application tools browser
	 openWithState: [ :browser |
		  browser selectPackage: package andTag: aPackageTagChange tagName ]
]
