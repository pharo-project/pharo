Extension { #name : #Class }

{ #category : #'*NewMethodFinder' }
Class >> allApprovedSelectors [
	
	^  (self approvedSelectors union: self superclass allApprovedSelectors) asSet
]

{ #category : #'*NewMethodFinder' }
Class >> allForbiddenSelectors [
	
	^  (self forbiddenSelectors union: self superclass allForbiddenSelectors) asSet
]

{ #category : #'*NewMethodFinder' }
Class >> allSelectorsToTest [
	"Returns all the selectors that the class approved, so which can be tested by the Method
	 Finder without problem. This set is the set of selectors that the class approved plus
	 all the selectors approved by its superclass unless if they are forbidden by the class."
	
	"^  self allApprovedSelectors difference: self allForbiddenSelectors"
	^ self approvedSelectors union: (self superclass allSelectorsToTest difference: self forbiddenSelectors) asSet.
]

{ #category : #'*NewMethodFinder' }
Class >> allSelectorsToTestWithArity: anInteger [ 
	"Returns all the selectors with a certain arity which are approved by the class so which 
	 can be tested in the Method Finder."
	
	^  self allSelectorsToTest select: [ :selector | selector numArgs = anInteger ]
]

{ #category : #'*NewMethodFinder' }
Class >> approvedSelectors [
	"The list of the selectors that the class approved. It's empty by default.
    An approved selector is a selector where the message send to the class could be
    tested by the Method Finder without problem. For instance, it does not modify 
    the environment and it does not touch to a globalvariable ."

	^ #()
]

{ #category : #'*NewMethodFinder' }
Class >> forbiddenSelectors [
	"The list of selector forbidden by the class. A forbidden selector will not be tested by
	 the Method Finder. So, a forbiden selector could be a method which modify the environment,
	 which modify a global variable, etc.
	"

	^ #()
]
