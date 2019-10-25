Class {
	#name : #SpMorphicBoxLayoutTest,
	#superclass : #TestCase,
	#instVars : [
		'instance'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #running }
SpMorphicBoxLayoutTest >> tearDown [
	instance
		ifNotNil: [ instance close.
			instance := nil ].
	super tearDown
]

{ #category : #tests }
SpMorphicBoxLayoutTest >> testAddWithPaddingHorizontal [
	| base presenter layout |

	base := SpBoxLayoutWithTableForTestPresenter new.
	presenter := SpLabelPresenter new.
	
	layout := SpBoxLayout newHorizontal
		add: presenter expand: false fill: false padding: 10;
		yourself.
		
	base buildAdapterWithSpecLayout: layout.

	"validate the widget is within a parent that keeps the widget with a padding"	
	self 
		assert: presenter adapter widget extent
		equals: (presenter adapter widget owner extent - (20@0))
]

{ #category : #tests }
SpMorphicBoxLayoutTest >> testAddWithPaddingVertical [
	| base presenter layout |

	base := SpBoxLayoutWithTableForTestPresenter new.
	presenter := SpLabelPresenter new.
	
	layout := SpBoxLayout newVertical
		add: presenter expand: false fill: false padding: 10;
		yourself.
		
	base buildAdapterWithSpecLayout: layout.

	"validate the widget is within a parent that keeps the widget with a padding"	
	self 
		assert: presenter adapter widget extent
		equals: (presenter adapter widget owner extent - (0@20))
]

{ #category : #tests }
SpMorphicBoxLayoutTest >> testClassIsDisplayedWithCorrectAlignment [
	| presenter table |

	presenter := SpBoxLayoutWithTableForTestPresenter new.
	instance := presenter openWithSpec.

	table := presenter table adapter widget.
	self assert: instance window left <= table left.
	self assert: table left <= table submorphs first left
]

{ #category : #tests }
SpMorphicBoxLayoutTest >> testNestedLayoutModifiesHeightOfTopLayout [
	| base presenter layout |

	base := SpBoxLayoutWithTableForTestPresenter new.
	presenter := SpLabelPresenter new
		label: 'Test';
		yourself.
	
	layout := SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal 
				add: presenter
				expand: false
				fill: false
				padding: 0) 
			expand: false 
			fill: false 
			padding: 0;
		yourself.
		
	base buildAdapterWithSpecLayout: layout.

	"validate the widget is within a parent that keeps the widget with a padding"	
	self 
		assert: presenter adapter widget owner owner height  
		equals: presenter adapter widget height
]
