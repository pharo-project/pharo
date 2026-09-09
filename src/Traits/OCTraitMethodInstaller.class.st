Class {
	#name : 'OCTraitMethodInstaller',
	#superclass : 'OCAnnouncingMethodInstaller',
	#category : 'Traits',
	#package : 'Traits'
}

{ #category : 'installing' }
OCTraitMethodInstaller >> addAndClassifyMethod: compiledMethod into: behavior [
	"When a new methods is added, I add it to the localMethodDict and also propagate the changes to my users"

	<reflection: 'Class structural modification - Selector/Method modification'>
	| selector |
	selector := compiledMethod selector.
	behavior localMethodDict at: selector put: compiledMethod.

	super addAndClassifyMethod: compiledMethod into: behavior.

	TraitChange addSelector: selector on: behavior
]
