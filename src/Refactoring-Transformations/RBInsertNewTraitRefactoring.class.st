"
I am a refactoring for creating a new trait.

I use the preconditions of my superclass.
"
Class {
	#name : 'RBInsertNewTraitRefactoring',
	#superclass : 'RBInsertNewBehaviorRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'transforming' }
RBInsertNewTraitRefactoring >> privateTransform [

	self model defineTrait: [ :aBuilder |
		aBuilder
			name: className;
			package: packageName;
			tag: tagName;
			comment: self comment ]
	traitComposition: self traitComposition
]
