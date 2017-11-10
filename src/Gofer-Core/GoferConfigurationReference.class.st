"
A GoferPackageReference refers to the latest version of a Monticello package representing a Metacello Configuration.
"
Class {
	#name : #GoferConfigurationReference,
	#superclass : #GoferPackageReference,
	#category : #Gofer-Core
}

{ #category : #accessing }
GoferConfigurationReference >> configurationClass [
	^ Smalltalk globals at: self configurationName asSymbol
]

{ #category : #accessing }
GoferConfigurationReference >> configurationName [
	^ 'ConfigurationOf', name
]

{ #category : #testing }
GoferConfigurationReference >> isConfigurationReference [
	^ true
]

{ #category : #accessing }
GoferConfigurationReference >> name [
	^ self configurationName 
]

{ #category : #accessing }
GoferConfigurationReference >> packageName [
	^ self configurationName
]

{ #category : #accessing }
GoferConfigurationReference >> project [
	^ self configurationClass project
]
