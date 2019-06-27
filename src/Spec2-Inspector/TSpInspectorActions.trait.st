"
I contain common actions for inspectors and inspector elements/wrappers
"
Trait {
	#name : #TSpInspectorActions,
	#category : #'Spec2-Inspector-Traits'
}

{ #category : #actions }
TSpInspectorActions >> browseSelectedObject [
	self selectedObjectDo: [ :anObject | anObject browse ]
]

{ #category : #actions }
TSpInspectorActions >> browseSelectedObjectClass [
	self selectedObjectDo: [ :anObject | Smalltalk tools browser openOnClass: anObject class ]
]

{ #category : #actions }
TSpInspectorActions >> browseSelectedObjectClassHierarchy [
	self selectedObjectDo: [ :anObject | anObject class browseHierarchy ]
]

{ #category : #actions }
TSpInspectorActions >> exploreSelectedObject [
	self selectedObjectDo: [ :anObject | anObject inspect ].
]

{ #category : #actions }
TSpInspectorActions >> inspectInNewWindow: anObject [

	anObject inspect
]

{ #category : #actions }
TSpInspectorActions >> inspectSelectedObjectInNewWindow [
	self selectedObjectDo: [ :anObject | self inspectInNewWindow: anObject ].
]
