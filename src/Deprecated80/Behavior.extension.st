Extension { #name : #Behavior }

{ #category : #'*Deprecated80' }
Behavior >> subclassInstVarNames [
	"Answer a Set of the names of the receiver's subclasses' instance variables."

	self
		deprecated:
			'This method is dead code in Pharo and will be removed. You can run (aClass subClasses flatCollect: instVarNames)'.
	^ self allSubclasses
		flatCollectAsSet: [ :aSubclass | aSubclass instVarNames ]
]
