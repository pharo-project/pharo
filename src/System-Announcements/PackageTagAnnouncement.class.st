"
My classes represent package tags modifications in the system.
"
Class {
	#name : #PackageTagAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'package',
		'tag'
	],
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'instance creation' }
PackageTagAnnouncement class >> in: aPackage tag: aSymbol [
	^self new
		package: aPackage;
		tag: aSymbol
]

{ #category : #'class initialization' }
PackageTagAnnouncement class >> initialize [

	self deprecatedAliases: { 'ClassTagAnnouncement' }
]

{ #category : #testing }
PackageTagAnnouncement >> affectsPackage: aPackage [
	^package == aPackage
]

{ #category : #testing }
PackageTagAnnouncement >> affectsPackages [
	^true
]

{ #category : #accessing }
PackageTagAnnouncement >> package [
	^ package
]

{ #category : #accessing }
PackageTagAnnouncement >> package: anObject [
	package := anObject
]

{ #category : #accessing }
PackageTagAnnouncement >> packagesAffected [
	^{self package}
]

{ #category : #accessing }
PackageTagAnnouncement >> tag [
	^ tag
]

{ #category : #accessing }
PackageTagAnnouncement >> tag: anObject [
	tag := anObject
]
