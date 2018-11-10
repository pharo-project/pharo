"
DEPRECATED: Replace it with its ancestor.
"
Class {
	#name : #IceTipExtendedVersionBrowser,
	#superclass : #IceTipVersionHistoryBrowser,
	#category : #'Iceberg-TipUI-View-Versions'
}

{ #category : #deprecation }
IceTipExtendedVersionBrowser class >> isDeprecated [
	^ true
]

{ #category : #'instance creation' }
IceTipExtendedVersionBrowser class >> repository: aRepository method: aMethod [
	"This method needs to be deprecated"
	
	^ self onRepository: aRepository method: aMethod
]

{ #category : #showing }
IceTipExtendedVersionBrowser >> open [
	self openWithSpec
]
