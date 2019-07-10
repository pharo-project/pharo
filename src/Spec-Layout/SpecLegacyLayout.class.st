Class {
	#name : #SpecLegacyLayout,
	#superclass : #Object,
	#category : #'Spec-Layout-Layouts'
}

{ #category : #deprecation }
SpecLegacyLayout class >> isDeprecated [
	"Layouts from Spec 1 will be removed in Pharo 9 because the implementation is not good. Please use the new SpecExecutableLayout subclasses instead"

	^ true
]

{ #category : #building }
SpecLegacyLayout >> buildAdapterFor: aPresenter bindings: adapterBindings [
	"Provides polymorphism with executable layouts."

	^ SpecInterpreter 
		interpretASpec: self 
		presenter: aPresenter
]

{ #category : #testing }
SpecLegacyLayout >> isSpecLayout [

	^ true
]
