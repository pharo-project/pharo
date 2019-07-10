"
I annotate browser tools that they should be opened as a tab in particular browser context.

I am not active for abstract classes. 
By default tool is abstract if it has subclasses. So for the cases when it is not true the tool class should override class side method #isAbstract
"
Class {
	#name : #ClyTabActivationStrategy,
	#superclass : #ClassAnnotation,
	#category : #'Calypso-Browser-Tabs'
}

{ #category : #operations }
ClyTabActivationStrategy >> createToolFor: aBrowser inContext: aBrowserContext [

	^self toolClass for: aBrowser inContext: aBrowserContext
]

{ #category : #testing }
ClyTabActivationStrategy >> isActiveInContext: aBrowserContext [
	^(super isActiveInContext: aBrowserContext)
		and: [ self toolClass shouldBeActivatedInContext: aBrowserContext ]
]

{ #category : #testing }
ClyTabActivationStrategy >> isForbidden [
	^self toolClass isAbstract
]

{ #category : #accessing }
ClyTabActivationStrategy >> toolClass [
	^annotatedClass
]
