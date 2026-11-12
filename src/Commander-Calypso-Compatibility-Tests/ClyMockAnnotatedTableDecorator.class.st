"
I am a table decorator annotated with the deprecated ClyTableDecorationStrategyAnnotation, to test that the annotation is still applied.
"
Class {
	#name : 'ClyMockAnnotatedTableDecorator',
	#superclass : 'Object',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'testing' }
ClyMockAnnotatedTableDecorator class >> isAbstract [

	^ false
]

{ #category : 'annotations' }
ClyMockAnnotatedTableDecorator class >> mockDecorationStrategy [

	<classAnnotation>
	^ ClyTableDecorationStrategyAnnotation for: ClyMockAnnotatedToolContext
]

{ #category : 'accessing' }
ClyMockAnnotatedTableDecorator class >> priority [

	^ 3
]

{ #category : 'testing' }
ClyMockAnnotatedTableDecorator class >> wantsDecorateTableCellInContext: aToolContext [

	^ true
]
