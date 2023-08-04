"
I am announced when new package tag is added into package
"
Class {
	#name : #PackageTagAdded,
	#superclass : #PackageTagAnnouncement,
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'class initialization' }
PackageTagAdded class >> initialize [

	self deprecatedAliases: { #ClassTagAdded . #CategoryAdded }
]
