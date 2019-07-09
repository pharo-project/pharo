"
I contain common actions for inspectors and inspector elements/wrappers
"
Trait {
	#name : #TEyeInspectorActions,
	#category : #'Spec2-Inspector-Traits'
}

{ #category : #actions }
TEyeInspectorActions >> browseSelectedObject [
	self selectedObjectDo: [ :anObject | anObject browse ]
]

{ #category : #actions }
TEyeInspectorActions >> browseSelectedObjectClass [
	self selectedObjectDo: [ :anObject | Smalltalk tools browser openOnClass: anObject class ]
]

{ #category : #actions }
TEyeInspectorActions >> browseSelectedObjectClassHierarchy [
	self selectedObjectDo: [ :anObject | anObject class browseHierarchy ]
]

{ #category : #actions }
TEyeInspectorActions >> exploreSelectedObject [
	self selectedObjectDo: [ :anObject | anObject inspect ].
]

{ #category : #actions }
TEyeInspectorActions >> inspectInNewWindow: anObject [

	anObject inspect
]

{ #category : #actions }
TEyeInspectorActions >> inspectSelectedObjectInNewWindow [
	self selectedObjectDo: [ :anObject | self inspectInNewWindow: anObject ].
]
