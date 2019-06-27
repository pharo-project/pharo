"
A special tree inspector that displays the submorph hierarchy of the inspected morph.
"
Class {
	#name : #SpEyeViewHierarchyInspector,
	#superclass : #SpEyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeViewHierarchyInspector class >> label [
	^ 'Morph View Hierarchy Inspector'
]

{ #category : #api }
SpEyeViewHierarchyInspector >> childrenForObject: aMorph [
	^ aMorph submorphs collect: [ :each |
		SpDynamicEyeElement
			host: aMorph
			value: each
		 ]
]

{ #category : #api }
SpEyeViewHierarchyInspector >> roots [
	^ self childrenForObject: self object
]
