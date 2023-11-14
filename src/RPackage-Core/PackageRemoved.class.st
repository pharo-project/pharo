"
I am a public announcement sent when a new Package is unregistred ( kind  of removed from the system )
"
Class {
	#name : 'PackageRemoved',
	#superclass : 'PackageAnnouncement',
	#category : 'RPackage-Core-Announcements',
	#package : 'RPackage-Core',
	#tag : 'Announcements'
}

{ #category : 'class initialization' }
PackageRemoved class >> initialize [
	self deprecatedAliases: { #RPackageUnregistered }
]
