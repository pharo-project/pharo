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
	"I define the vertical alignment of the widgets in the layout.
	 aligns can be start, center and end. 
	 To set this value, you can use vAlignStart, vAlignCenter, vAlignEnd"

	^ hAlign
]

{ #category : #accessing }
SpTAlignable >> hAlign: anAlignment [
	"Sets the align property. See #hAlign to details"
	
	hAlign := anAlignment
]

{ #category : #accessing }
SpTAlignable >> hAlignCenter [

	self hAlign: SpLayoutWidgetAlignment center
]

{ #category : #accessing }
SpTAlignable >> hAlignEnd [

	self hAlign: SpLayoutWidgetAlignment end
]

{ #category : #accessing }
SpTAlignable >> hAlignStart [

	self hAlign: SpLayoutWidgetAlignment start
]

{ #category : #testing }
SpTAlignable >> hasAlignment [

	^ vAlign notNil or: [ hAlign notNil ]
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
	"Sets the align property. See #vAlign to details"

	vAlign := anAlignment
]

{ #category : #accessing }
SpTAlignable >> vAlignCenter [

	self vAlign: SpLayoutWidgetAlignment center
]

{ #category : #accessing }
SpTAlignable >> vAlignEnd [

	self vAlign: SpLayoutWidgetAlignment end
]

{ #category : #accessing }
SpTAlignable >> vAlignStart [

	self vAlign: SpLayoutWidgetAlignment start
]
