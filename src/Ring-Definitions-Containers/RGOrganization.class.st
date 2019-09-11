"
An organization with categories
"
Class {
	#name : #RGOrganization,
	#superclass : #RGAbstractContainer,
	#category : #'Ring-Definitions-Containers-Base'
}

{ #category : #accessing }
RGOrganization >> categories [
	"Retrieves categories in the system (tag-packages)"

	^self elements at: #categories ifAbsent:[ #() ]
]

{ #category : #accessing }
RGOrganization >> categories: aCollection [
	"Set the categories collection"

	self elements at: #categories put: aCollection
]

{ #category : #accessing }
RGOrganization >> fullName [

	^#organization
]

{ #category : #initialization }
RGOrganization >> initialize [

	super initialize.
	self categories: OrderedCollection new.
]

{ #category : #testing }
RGOrganization >> isOrganization [

	^true
]

{ #category : #testing }
RGOrganization >> isSameRevisionAs: aRGOrganization [
	"This method look for equality of the properties of the receiver"

	^(self class = aRGOrganization class)
		and:[ self categories = aRGOrganization categories ]
]
