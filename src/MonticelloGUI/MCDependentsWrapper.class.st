"
Specialized list item wrapper for dependents
"
Class {
	#name : #MCDependentsWrapper,
	#superclass : #ListItemWrapper,
	#category : #MonticelloGUI
}

{ #category : #comparing }
MCDependentsWrapper >> = anMCDependentsWrapper [

	self class = anMCDependentsWrapper class ifFalse: [^ false].
	^ (self item package name = anMCDependentsWrapper item package name) and: [self model = anMCDependentsWrapper model].

]

{ #category : #converting }
MCDependentsWrapper >> asString [
	^item description
]

{ #category : #accessing }
MCDependentsWrapper >> contents [
	| list workingCopies |
	workingCopies := model unsortedWorkingCopies.
	list := item requiredPackages collect: 
					[:each | 
					workingCopies detect: [:wc | wc package = each] ifNone: [nil]]
				thenSelect: [:x | x notNil].
	^list collect: [:each | self class with: each model: model]
]

{ #category : #accessing }
MCDependentsWrapper >> hasContents [
	^item requiredPackages isEmpty not
]

{ #category : #comparing }
MCDependentsWrapper >> hash [
	^ (self item package name hash * 37) + self model hash 

]
