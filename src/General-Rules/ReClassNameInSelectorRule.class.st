"
This smell arises when the class name is found in a selector. This is redundant since to call the you must already refer to the class name.

For example, #openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser.
"
Class {
	#name : 'ReClassNameInSelectorRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'testing - interest' }
ReClassNameInSelectorRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReClassNameInSelectorRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReClassNameInSelectorRule class >> rationale [
	^ 'Checks for the class name in a selector. This is redundant since to call the you must already refer to the class name. For example, openHierarchyBrowserFrom: is a redundant name for HierarchyBrowser. Avoiding selector including class name gives a chance to have more polymorphic methods.'
]

{ #category : 'accessing' }
ReClassNameInSelectorRule class >> ruleName [
	^ 'Redundant class name in selector'
]

{ #category : 'manifest' }
ReClassNameInSelectorRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ClassNameInSelectorRule'
]

{ #category : 'running' }
ReClassNameInSelectorRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| className classNameIndex |
	(aMethod methodClass isMeta) ifFalse: [ ^ self ].
	className := aMethod methodClass soleInstance name.
	classNameIndex := aMethod selector
		indexOfSubCollection: className
		startingAt: 1.

	classNameIndex > 0 ifTrue: [ aCriticBlock cull: (
		(self
			critiqueFor: aMethod
			withInterval: (classNameIndex to: classNameIndex + className size ))
			tinyHint: className;
			yourself) ]
]

{ #category : 'running' }
ReClassNameInSelectorRule >> critiqueFor: aMethod withInterval: anInterval [

	^ ReTrivialCritique
		withAnchor: (ReIntervalSourceAnchor
			entity: aMethod
			interval: anInterval)
		by: self
]
