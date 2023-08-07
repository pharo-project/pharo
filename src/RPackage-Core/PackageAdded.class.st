"
I am a public announcement sent when a new RPackage is created
"
Class {
	#name : #PackageAdded,
	#superclass : #PackageAnnouncement,
	#category : #'RPackage-Core-Announcements'
}

{ #category : #'class initialization' }
PackageAdded class >> initialize [
	self deprecatedAliases: { #RPackageRegistered }
]
