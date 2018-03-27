Extension { #name : #ChangeRecord }

{ #category : #'*Ring-Deprecated-Core-Kernel' }
ChangeRecord >> asRingDefinition [
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	self type == #method
		ifTrue: [ ^self createMethodDefinition ].
	self type == #classComment
		ifTrue: [ ^self createCommentDefinition ].
	^nil
]

{ #category : #'*Ring-Deprecated-Core-Kernel' }
ChangeRecord >> createCommentDefinition [
	"Retrieves a RGCommentDefinition object based on the data of the receiver"

	^ (RGCommentDefinition class: self commentClass)
		content: self string;
		stamp: stamp;
		yourself 
]

{ #category : #'*Ring-Deprecated-Core-Kernel' }
ChangeRecord >> createMethodDefinition [
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	^(RGMethodDefinition named: self methodSelector)
		parentName: self methodClassName;
		isMetaSide: meta;  
		sourceCode: self string;
		protocol: category;
		stamp: stamp;
		yourself
]
