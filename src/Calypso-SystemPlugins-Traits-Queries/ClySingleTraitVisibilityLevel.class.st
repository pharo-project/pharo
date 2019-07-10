"
I represent visibility of methods of concrete trait.

I am same as ClySuperclassVisibilityLevel. But I was introduced to allow decorate visibility item with special trait icon
"
Class {
	#name : #ClySingleTraitVisibilityLevel,
	#superclass : #ClyMethodVisibilityLevel,
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClySingleTraitVisibilityLevel >> asTraitVisibility [
	^self
]
