tests
testSimple
	| wrapper textMorph |
	wrapper := GLMWrapper new.
	wrapper show: [ :a | a text ].
	window := wrapper openOn: 42.
	textMorph := self findWherePresentationIs: wrapper defaultPane presentations first in: window.
	self assert: textMorph text = '42'