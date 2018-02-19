"
A StickynessBugz is for mantis #5500 rectangles and ellipses don't act sticky when rotated even when they are..

Instance Variables

"
Class {
	#name : #StickynessBugsTest,
	#superclass : #MorphicUIBugTest,
	#category : #'Morphic-Tests-Bugs'
}

{ #category : #tests }
StickynessBugsTest >> testForTiltedStickyness [

	| m |
	m := Morph new openCenteredInWorld.
	cases := Array with: m.	"save for tear down."
	self assert: m topRendererOrSelf isSticky not.
	m beSticky.
	self assert: m topRendererOrSelf isSticky.
	m addFlexShell.
	cases := Array with: m topRendererOrSelf.
	m topRendererOrSelf rotationDegrees: 45.0.
	self assert: m topRendererOrSelf isSticky.
	m beUnsticky.
	self assert: m topRendererOrSelf isSticky not.
	m topRendererOrSelf delete.
	^ true
]
