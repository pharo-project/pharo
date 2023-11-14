"
I am a public announcement sent when a new Package is created
"
Class {
	#name : 'PackageAdded',
	#superclass : 'PackageAnnouncement',
	#category : 'RPackage-Core-Announcements',
	#package : 'RPackage-Core',
	#tag : 'Announcements'
}

{ #category : 'class initialization' }
PackageAdded class >> initialize [
	self deprecatedAliases: { #RPackageRegistered }
]
