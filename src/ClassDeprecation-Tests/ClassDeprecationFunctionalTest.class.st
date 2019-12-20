Class {
	#name : #ClassDeprecationFunctionalTest,
	#superclass : #ClassDeprecationTest,
	#instVars : [
		'referencingOriginalResult',
		'referencingTransformedResult'
	],
	#category : #'ClassDeprecation-Tests'
}

{ #category : #tests }
ClassDeprecationFunctionalTest >> assertDoesNotTransformSources [
	"Asserts that #references was not instrumented and hence will not transform the source after execution."

	self
		assert: referencingClass new references
		equals: referencingOriginalResult.
	self
		assert: (referencingClass compiledMethodAt: #references) sourceCode
		equals: referencingOriginalSource
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> assertDoesTransformSources [
	"Asserts that #references was instrumented and hence will transform the source after execution."

	"first execution should not replace value, but replace source"
	self
		assert: referencingClass new references
		equals: referencingOriginalResult.
	self
		assert: (referencingClass compiledMethodAt: #references) sourceCode
		equals: referencingTransformedSource.
	"second execution should replace value, but not source"
	self
		assert: referencingClass new references
		equals: referencingTransformedResult.
	self
		assert: (referencingClass compiledMethodAt: #references) sourceCode
		equals: referencingTransformedSource
]

{ #category : #running }
ClassDeprecationFunctionalTest >> setUp [
	super setUp.
	referencingOriginalResult := {deprecatedClass.
	1.
	referencingClass.
	deprecatedClass.
	referencingClass}.
	referencingTransformedResult := {replacementClass.
	1.
	referencingClass.
	replacementClass.
	referencingClass}
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testDeprecateEnableReference [
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	ClassDeprecationMetaLink enable.
	referencingClass compile: referencingOriginalSource.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testDeprecateReferenceEnable [
	"first execution should not replace value, but replace source"
	"second execution should replace value, but not source"
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	referencingClass compile: referencingOriginalSource.
	ClassDeprecationMetaLink enable.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testEnableDeprecateReference [
	ClassDeprecationMetaLink enable.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	referencingClass compile: referencingOriginalSource.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testEnableReferenceDeprecate [
	ClassDeprecationMetaLink enable.
	referencingClass compile: referencingOriginalSource.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testReferenceDeprecateEnable [
	referencingClass compile: referencingOriginalSource.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	ClassDeprecationMetaLink enable.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testReferenceDeprecateEnableDisable [
	referencingClass compile: referencingOriginalSource.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	ClassDeprecationMetaLink
		enable;
		disable.
	self assertDoesNotTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testReferenceDeprecateEnableUndeprecated [
	referencingClass compile: referencingOriginalSource.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	ClassDeprecationMetaLink enable.
	self deprecateNotClass: deprecatedClass.
	self assertDoesNotTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testReferenceEnableDeprecate [
	referencingClass compile: referencingOriginalSource.
	ClassDeprecationMetaLink enable.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	self assertDoesTransformSources
]

{ #category : #tests }
ClassDeprecationFunctionalTest >> testSetUp [
	self
		deny: referencingOriginalSource
		equals: referencingTransformedSource.
	self
		deny: referencingOriginalResult
		equals: referencingTransformedResult
]
