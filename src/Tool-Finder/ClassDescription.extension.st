Extension { #name : #ClassDescription }

{ #category : #'*Tool-Finder' }
ClassDescription >> allSelectorsToTestInMethodFinder [
	"Returns all the selectors that the class approved, so which can be tested by the MethodFinder without problem. This set is the set of selectors that the class approved plus all the selectors approved by its superclass unless if they are forbidden by the class."

	^ (self approvedSelectorsForMethodFinder union: self superclass allSelectorsToTestInMethodFinder)
			difference: self forbiddenSelectorsForMethodFinder asSet
]

{ #category : #'*Tool-Finder' }
ClassDescription >> allSelectorsToTestInMethodFinderWithArity: anInteger [
	"Returns all the selectors with a certain arity which are approved by the class so which can be tested in the Method Finder."

	^  self allSelectorsToTestInMethodFinder select: [ :selector | selector numArgs = anInteger ]
]

{ #category : #'*Tool-Finder' }
ClassDescription >> approvedSelectorsForMethodFinder [
	"The list of the selectors that the class approved. It's empty by default.
    An approved selector is a selector where the message send to the class could be
    tested by the Method Finder without problem. For instance, it does not modify
    the environment and it does not touch to a globalvariable ."

	^ #(isNil)
]

{ #category : #'*Tool-Finder' }
ClassDescription >> forbiddenSelectorsForMethodFinder [
	"The list of selector forbidden by the class. A forbidden selector will not be tested by the Method Finder. So, a forbiden selector could be a method which modifies the environment, a global variable, etc."

	^ #()
]
