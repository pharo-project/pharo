"
I'm a base model for tip UI. 
Each tip component refers to one (and just one) model, which will provide model actions and access from presenters to backend.
Models can be cached using #beCached.
"
Class {
	#name : #IceTipModel,
	#superclass : #Object,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #converting }
IceTipModel >> beCached [
	^ IceTipCachedModel for: self
]

{ #category : #testing }
IceTipModel >> matches: aString [
	"Override this if you want to make this model discoverable"
	^ false
]

{ #category : #accessing }
IceTipModel >> repositoryModel [
	"this model does not provides a repository model"
	^ nil 
]

{ #category : #accessing }
IceTipModel >> reset [
	"to provide polymorphism with cached version"
]

{ #category : #private }
IceTipModel >> toErrorMessage: aString [
	^ aString asText allBold makeAllColor: self colorError
]
