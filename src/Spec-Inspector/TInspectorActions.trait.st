"
I contain common actions for inspectors and inspector elements/wrappers
"
Trait {
	#name : #TInspectorActions,
	#category : #Spec-Inspector
}

{ #category : #actions }
TInspectorActions >> browseSelectedObject [
	self selectedObjectDo: [ :anObject | anObject browse ]
]

{ #category : #actions }
TInspectorActions >> browseSelectedObjectClass [
	self selectedObjectDo: [ :anObject | Smalltalk tools browser openOnClass: anObject class ]
]

{ #category : #actions }
TInspectorActions >> browseSelectedObjectClassHierarchy [
	self selectedObjectDo: [ :anObject | anObject class browseHierarchy ]
]

{ #category : #actions }
TInspectorActions >> exploreSelectedObject [
	self selectedObjectDo: [ :anObject | anObject inspect ].
]

{ #category : #actions }
TInspectorActions >> inspectInNewWindow: anObject [

	anObject inspect
]

{ #category : #actions }
TInspectorActions >> inspectSelectedObjectInNewWindow [
	self selectedObjectDo: [ :anObject | self inspectInNewWindow: anObject ].
]
