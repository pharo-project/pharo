"
I add alignment properties to layouts. 
Alignments can be added by layout basis (see SpBoxLayout) or widget basis (see SpGridConstraints).
"
Trait {
	#name : #SpTAlignable,
	#instVars : [
		'vAlign',
		'hAlign'
	],
	#category : #'Spec2-Layout-Base'
}

{ #category : #accessing }
SpTAlignable >> hAlign [
	"I define the horizontal alignment of the widgets in the layout.
	 aligns can be start, center and end. 
	 To set this value, you can use hAlignStart, hAlignCenter, hAlignEnd"

	^ hAlign
]

{ #category : #accessing }
SpTAlignable >> hAlign: anAlignment [
	"Sets the horizontal align property. See #hAlign to details"
	
	hAlign := anAlignment
]

{ #category : #accessing }
SpTAlignable >> hAlignCenter [
	"Sets the horizontal align property to CENTER. See #hAlign for details"

	self hAlign: SpLayoutWidgetAlignment center
]

{ #category : #accessing }
SpTAlignable >> hAlignEnd [
	"Sets the horizontal align property to END. See #hAlign for details"

	self hAlign: SpLayoutWidgetAlignment end
]

{ #category : #accessing }
SpTAlignable >> hAlignStart [
	"Sets the horizontal align property to START. See #hAlign for details"

	self hAlign: SpLayoutWidgetAlignment start
]

{ #category : #testing }
SpTAlignable >> hasAlignment [
	"Answer whether the component has any alignment defined"

	^ self hasVAlign or: [ self hasHAlign ]
]

{ #category : #testing }
SpTAlignable >> hasHAlign [
	"Answer whether the component has an horizontal alignment defined"

	^ hAlign notNil
]

{ #category : #testing }
SpTAlignable >> hasVAlign [
	"Answer whether the component has a vertical alignment defined"

	^ vAlign notNil
]

{ #category : #accessing }
SpTAlignable >> vAlign [
	"I define the vertical alignment of the widgets in the layout.
	 aligns can be start, center and end. 
	 To set this value, you can use vAlignStart, vAlignCenter, vAlignEnd"

	^ vAlign
]

{ #category : #accessing }
SpTAlignable >> vAlign: anAlignment [
	"Sets the vertical align property. See #vAlign to details"

	vAlign := anAlignment
]

{ #category : #accessing }
SpTAlignable >> vAlignCenter [
	"Sets the vertical align property to CENTER. See #vAlign for details"

	self vAlign: SpLayoutWidgetAlignment center
]

{ #category : #accessing }
SpTAlignable >> vAlignEnd [
	"Sets the vertical align property to END. See #vAlign for details"

	self vAlign: SpLayoutWidgetAlignment end
]

{ #category : #accessing }
SpTAlignable >> vAlignStart [
	"Sets the vertical align property to START. See #vAlign for details"

	self vAlign: SpLayoutWidgetAlignment start
]
