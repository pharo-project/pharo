"
I am a public announcement sent when a new RPackage is unregistred ( kind  of removed from the system )
"
Class {
	#name : #PackageRemoved,
	#superclass : #PackageAnnouncement,
	#category : #'RPackage-Core-Announcements'
}

{ #category : #'class initialization' }
PackageRemoved class >> initialize [
	self deprecatedAliases: { #RPackageUnregistered }
]
