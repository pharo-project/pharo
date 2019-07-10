"
A special tree inspector that displays the submorph hierarchy of the inspected morph.
"
Class {
	#name : #EyeViewHierarchyInspector,
	#superclass : #EyeTreeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeViewHierarchyInspector class >> label [
	^ 'Morph View Hierarchy Inspector'
]

{ #category : #api }
EyeViewHierarchyInspector >> childrenForObject: aMorph [
	^ aMorph submorphs collect: [ :each |
		DynamicEyeElement
			host: aMorph
			value: each
		 ]
]

{ #category : #api }
EyeViewHierarchyInspector >> roots [
	^ self childrenForObject: self object
]
