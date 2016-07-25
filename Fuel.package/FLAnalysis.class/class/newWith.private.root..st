instance creation
newWith: aMapper private: privateMapper root: anObject

	^self basicNew
		initializeWith: aMapper private: privateMapper root: anObject;
		yourself.