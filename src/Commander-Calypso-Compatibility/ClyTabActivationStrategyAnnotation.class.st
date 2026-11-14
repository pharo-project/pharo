"
Deprecated: tabs are declared with the <calypsoCommands> pragma (ClyCommandMap>>#tabs). I am kept, in the deprecated Commander-Calypso-Compatibility package, for the projects which still use me, and I will be removed in Pharo 16. doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository explains the migration.

I annotate browser tools that they should be opened as a tab in particular browser context.

I am not active for abstract classes. 
By default tool is abstract if it has subclasses. So for the cases when it is not true the tool class should override class side method #isAbstract
"
Class {
	#name : 'ClyTabActivationStrategyAnnotation',
	#superclass : 'ClassAnnotation',
	#category : 'Commander-Calypso-Compatibility-Annotations',
	#package : 'Commander-Calypso-Compatibility',
	#tag : 'Annotations'
}

{ #category : 'accessing' }
ClyTabActivationStrategyAnnotation class >> interactionName [
	"ClyCommandRegistry adds my instances to the tabs declared with <calypsoCommands> (ClyCommandMap>>#tabs)."

	^ #tabs
]

{ #category : 'operations' }
ClyTabActivationStrategyAnnotation >> createToolFor: aBrowser inContext: aBrowserContext [

	^self toolClass for: aBrowser inContext: aBrowserContext
]

{ #category : 'testing' }
ClyTabActivationStrategyAnnotation >> isActiveInContext: aBrowserContext [
	^(super isActiveInContext: aBrowserContext)
		and: [ self toolClass shouldBeActivatedInContext: aBrowserContext ]
]

{ #category : 'testing' }
ClyTabActivationStrategyAnnotation >> isForbidden [
	^self toolClass isAbstract
]

{ #category : 'accessing' }
ClyTabActivationStrategyAnnotation >> toolClass [
	^annotatedClass
]
