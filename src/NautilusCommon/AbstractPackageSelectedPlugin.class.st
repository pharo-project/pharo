"
AbstractKeyPressedPlugin is an abstraction of plugin which react when a package is selected
"
Class {
	#name : #AbstractPackageSelectedPlugin,
	#superclass : #AbstractNautilusPlugin,
	#category : #NautilusCommon-Plugin
}

{ #category : #announcement }
AbstractPackageSelectedPlugin >> packageSelected: anAnnouncement [

	^ self subclassResponsibility
]

{ #category : #registration }
AbstractPackageSelectedPlugin >> registerTo: aModel [
	aModel announcer
		when: NautilusPackageSelected send: #packageSelected: to: self
]
