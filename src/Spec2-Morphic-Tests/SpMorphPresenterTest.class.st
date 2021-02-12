Class {
	#name : #SpMorphPresenterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Morphic-Tests'
}

{ #category : #accessing }
SpMorphPresenterTest >> classToTest [

	^ SpMorphPresenter
]

{ #category : #tests }
SpMorphPresenterTest >> testChangingHelpAffectTheWidget [
	"It does not has sense here"

	self skip
]

{ #category : #tests }
SpMorphPresenterTest >> testChangingMorphUpdatesContents [
	| m1 m2 |

	m1 := Morph new color: Color red.
	m2 := Morph new color: Color green.
	self assert: presenter morph isNil.
	self openInstance.

	presenter morph: m1.
	backendForTest waitUntilUIRedrawed.
	self assert: presenter adapter widget equals: m1.
	presenter morph: m2.
	backendForTest waitUntilUIRedrawed.
	self assert: presenter adapter widget equals: m2
]
